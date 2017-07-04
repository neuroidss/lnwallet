package com.lightning.wallet.ln

import fr.acinq.bitcoin.Crypto._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._

import com.lightning.wallet.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.wallet.ln.Tools.LightningMessages
import com.lightning.wallet.ln.MSat.satFactor


sealed trait Command
case object CMDShutdown extends Command
case object CMDCommitSig extends Command

case class CMDOpenChannel(localParams: LocalParams, temporaryChannelId: BinaryData, initialFeeratePerKw: Long,
                          pushMsat: Long, remoteInit: Init, fundingAmountSat: Long) extends Command

trait CMDAddHtlc extends Command { val spec: OutgoingPaymentSpec }
case class PlainAddHtlc(spec: OutgoingPaymentSpec) extends CMDAddHtlc
case class SilentAddHtlc(spec: OutgoingPaymentSpec) extends CMDAddHtlc

case class CMDFailMalformedHtlc(id: Long, onionHash: BinaryData, code: Int) extends Command
case class CMDFulfillHtlc(id: Long, preimage: BinaryData) extends Command
case class CMDFailHtlc(id: Long, reason: BinaryData) extends Command

case class CMDSomethingSpent(tx: Transaction, isFunding: Boolean) extends Command
case class CMDSomethingConfirmed(tx: Transaction) extends Command
case class CMDFeerate(rate: Long) extends Command
case class CMDDepth(depth: Int) extends Command

// CHANNEL DATA

sealed trait ChannelData { val announce: NodeAnnouncement }
sealed trait HasCommitments { val commitments: Commitments }

case class InitData(announce: NodeAnnouncement) extends ChannelData
case class WaitAcceptData(announce: NodeAnnouncement, cmd: CMDOpenChannel) extends ChannelData
case class WaitFundingData(announce: NodeAnnouncement, cmd: CMDOpenChannel, accept: AcceptChannel) extends ChannelData

case class WaitFundingSignedData(announce: NodeAnnouncement, localParams: LocalParams, channelId: BinaryData,
                                 remoteParams: RemoteParams, fundingTx: Transaction, localSpec: CommitmentSpec,
                                 localCommitTx: CommitTx, remoteCommit: RemoteCommit) extends ChannelData

// All the data below will be stored

case class WaitFundingConfirmedData(announce: NodeAnnouncement, our: Option[FundingLocked],
                                    their: Option[FundingLocked], fundingTx: Transaction, commitments: Commitments,
                                    kind: String = "WaitFundingConfirmedData") extends ChannelData with HasCommitments

case class NormalData(announce: NodeAnnouncement, commitments: Commitments,
                      localShutdown: Option[Shutdown], remoteShutdown: Option[Shutdown] = None,
                      kind: String = "NormalData") extends ChannelData with HasCommitments

case class NegotiationsData(announce: NodeAnnouncement, commitments: Commitments,
                            localClosingSigned: ClosingSigned, localShutdown: Shutdown, remoteShutdown: Shutdown,
                            kind: String = "NegotiationsData") extends ChannelData with HasCommitments

// Storing all the tx types to be published

case class ClosingData(announce: NodeAnnouncement, commitments: Commitments, mutualClose: Seq[Transaction] = Nil,
                       localCommit: Seq[LocalCommitPublished] = Nil, remoteCommit: Seq[RemoteCommitPublished] = Nil,
                       nextRemoteCommit: Seq[RemoteCommitPublished] = Nil, revokedCommits: Seq[RevokedCommitPublished] = Nil,
                       kind: String = "ClosingData") extends ChannelData with HasCommitments

case class BroadcastStatus(relativeDelay: Option[Long], publishable: Boolean, tx: Transaction)
case class LocalCommitPublished(claimMainDelayedOutputTx: Seq[Transaction], htlcSuccessTxs: Seq[Transaction],
                                htlcTimeoutTxs: Seq[Transaction], claimHtlcSuccessTxs: Seq[Transaction],
                                claimHtlcTimeoutTxs: Seq[Transaction], commitTx: Transaction)

case class RemoteCommitPublished(claimMainOutputTx: Seq[Transaction], claimHtlcSuccessTxs: Seq[Transaction],
                                 claimHtlcTimeoutTxs: Seq[Transaction], commitTx: Transaction)

case class RevokedCommitPublished(claimMainOutputTx: Seq[Transaction], mainPenaltyTx: Seq[Transaction],
                                  claimHtlcTimeoutTxs: Seq[Transaction], htlcTimeoutTxs: Seq[Transaction],
                                  htlcPenaltyTxs: Seq[Transaction], commitTx: Transaction)

// COMMITMENTS

case class Htlc(incoming: Boolean, add: UpdateAddHtlc)
case class CommitmentSpec(feeratePerKw: Long, toLocalMsat: Long,
                          toRemoteMsat: Long, htlcs: Set[Htlc] = Set.empty)

object CommitmentSpec {
  def findHtlcById(cs: CommitmentSpec, id: Long, isIncoming: Boolean): Option[Htlc] =
    cs.htlcs.find(htlc => htlc.add.id == id && htlc.incoming == isIncoming)

  private def fulfill(cs: CommitmentSpec, in: Boolean, update: UpdateFulfillHtlc) = findHtlcById(cs, update.id, in) match {
    case Some(htlc) if htlc.incoming => cs.copy(toLocalMsat = cs.toLocalMsat + htlc.add.amountMsat, htlcs = cs.htlcs - htlc)
    case Some(htlc) => cs.copy(toRemoteMsat = cs.toRemoteMsat + htlc.add.amountMsat, htlcs = cs.htlcs - htlc)
    case None => cs
  }

  private def fail(cs: CommitmentSpec, in: Boolean, update: UpdateFailHtlc) = findHtlcById(cs, update.id, in) match {
    case Some(htlc) if htlc.incoming => cs.copy(toRemoteMsat = cs.toRemoteMsat + htlc.add.amountMsat, htlcs = cs.htlcs - htlc)
    case Some(htlc) => cs.copy(toLocalMsat = cs.toLocalMsat + htlc.add.amountMsat, htlcs = cs.htlcs - htlc)
    case None => cs
  }

  private def plus(htlc: Htlc, cs: CommitmentSpec) =
    if (htlc.incoming) cs.copy(htlcs = cs.htlcs + htlc, toRemoteMsat = cs.toRemoteMsat - htlc.add.amountMsat)
    else cs.copy(htlcs = cs.htlcs + htlc, toLocalMsat = cs.toLocalMsat - htlc.add.amountMsat)

  def reduce(cs: CommitmentSpec, localChanges: LightningMessages, remoteChanges: LightningMessages) = {
    val spec1 = (cs /: localChanges) { case (spec, add: UpdateAddHtlc) => plus(Htlc(incoming = false, add), spec) case (spec, _) => spec }
    val spec2 = (spec1 /: remoteChanges) { case (spec, add: UpdateAddHtlc) => plus(Htlc(incoming = true, add), spec) case (spec, _) => spec }

    val spec3 = (spec2 /: localChanges) {
      case (spec, msg: UpdateFailHtlc) => fail(spec, in = true, msg)
      case (spec, msg: UpdateFulfillHtlc) => fulfill(spec, in = true, msg)
      case (spec, u: UpdateFee) => spec.copy(feeratePerKw = u.feeratePerKw)
      case (spec, _) => spec
    }

    (spec3 /: remoteChanges) {
      case (spec, msg: UpdateFailHtlc) => fail(spec, in = false, msg)
      case (spec, msg: UpdateFulfillHtlc) => fulfill(spec, in = false, msg)
      case (spec, u: UpdateFee) => spec.copy(feeratePerKw = u.feeratePerKw)
      case (spec, _) => spec
    }
  }
}

case class LocalParams(chainHash: BinaryData, dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: Long,
                       channelReserveSat: Long, htlcMinimumMsat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int,
                       fundingPrivKey: PrivateKey, revocationSecret: Scalar, paymentKey: PrivateKey, delayedPaymentKey: Scalar,
                       defaultFinalScriptPubKey: BinaryData, shaSeed: BinaryData, isFunder: Boolean)

case class RemoteParams(dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: Long, channelReserveSatoshis: Long,
                        htlcMinimumMsat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int, fundingPubKey: PublicKey,
                        revocationBasepoint: Point, paymentBasepoint: Point, delayedPaymentBasepoint: Point,
                        globalFeatures: BinaryData, localFeatures: BinaryData)

case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig)
case class LocalCommit(index: Long, spec: CommitmentSpec, htlcTxsAndSigs: Seq[HtlcTxAndSigs], commitTx: CommitTx)
case class RemoteCommit(index: Long, spec: CommitmentSpec, txid: BinaryData, remotePerCommitmentPoint: Point)
case class HtlcTxAndSigs(txinfo: TransactionWithInputInfo, localSig: BinaryData, remoteSig: BinaryData)
case class Changes(proposed: LightningMessages, signed: LightningMessages, acked: LightningMessages)
object Changes { def all(c: Changes): LightningMessages = c.proposed ++ c.signed ++ c.acked }

case class Commitments(localParams: LocalParams, remoteParams: RemoteParams, localCommit: LocalCommit, remoteCommit: RemoteCommit,
                       localChanges: Changes, remoteChanges: Changes, localNextHtlcId: Long, remoteNextHtlcId: Long,
                       remoteNextCommitInfo: Either[WaitingForRevocation, Point], commitInput: InputInfo,
                       remotePerCommitmentSecrets: ShaHashesWithIndex, channelId: BinaryData)

object Commitments {
  def hasTimedoutOutgoingHtlcs(c: Commitments, blockHeight: Long): Boolean =
    c.localCommit.spec.htlcs.exists(htlc => !htlc.incoming && blockHeight >= htlc.add.expiry) ||
      c.remoteCommit.spec.htlcs.exists(htlc => htlc.incoming && blockHeight >= htlc.add.expiry)

  def localHasChanges(c: Commitments): Boolean = c.remoteChanges.acked.nonEmpty || c.localChanges.proposed.nonEmpty
  def remoteHasChanges(c: Commitments): Boolean = c.localChanges.acked.nonEmpty || c.remoteChanges.proposed.nonEmpty
  def hasNoPendingHtlcs(c: Commitments): Boolean = c.localCommit.spec.htlcs.isEmpty && c.remoteCommit.spec.htlcs.isEmpty

  def addRemoteProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.remoteChanges.proposed).using(_ :+ proposal)
  def addLocalProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.localChanges.proposed).using(_ :+ proposal)

  def getHtlcCrossSigned(commitments: Commitments, incomingRelativeToLocal: Boolean, htlcId: Long) = {
    val remoteSigned = CommitmentSpec.findHtlcById(commitments.localCommit.spec, htlcId, incomingRelativeToLocal)

    val localSigned = commitments.remoteNextCommitInfo match {
      case Left(wait) => CommitmentSpec.findHtlcById(wait.nextRemoteCommit.spec, htlcId, !incomingRelativeToLocal)
      case _ => CommitmentSpec.findHtlcById(commitments.remoteCommit.spec, htlcId, !incomingRelativeToLocal)
    }

    for {
      htlcOut <- remoteSigned
      htlcIn <- localSigned
    } yield htlcIn.add
  }

  def sendAdd(c: Commitments, cmd: CMDAddHtlc, blockLimit: Int) =
    if (cmd.spec.expiry <= blockLimit) throw ExtendedException(cmd)
    else if (cmd.spec.request.amount.get > LNParams.maxHtlcValue) throw ExtendedException(cmd)
    else if (cmd.spec.amountWithFee < c.remoteParams.htlcMinimumMsat) throw ExtendedException(cmd)
    else if (cmd.spec.request.paymentHash.size != 32) throw ExtendedException(cmd)
    else {

      // Let's compute the current commitment *as seen by them* with this change taken into account
      val add = UpdateAddHtlc(c.channelId, c.localNextHtlcId, cmd.spec.amountWithFee, cmd.spec.expiry,
        cmd.spec.request.paymentHash, cmd.spec.onion.packet.serialize)

      val c1 = addLocalProposal(c, add).modify(_.localNextHtlcId).using(_ + 1)
      val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
      val fees = if (c1.localParams.isFunder) Scripts.commitTxFee(Satoshi(c1.remoteParams.dustLimitSatoshis), reduced).amount else 0
      val htlcValueInFlightOverflow = reduced.htlcs.map(_.add.amountMsat).sum > c1.remoteParams.maxHtlcValueInFlightMsat
      val feesOverflow = reduced.toRemoteMsat / satFactor - c1.remoteParams.channelReserveSatoshis - fees < 0
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.remoteParams.maxAcceptedHtlcs

      // The rest of the guards
      if (htlcValueInFlightOverflow) throw ExtendedException(cmd)
      else if (acceptedHtlcsOverflow) throw ExtendedException(cmd)
      else if (feesOverflow) throw ExtendedException(cmd)
      else c1 -> add
    }

  private def receiveAdd(c: Commitments, add: UpdateAddHtlc, blockLimit: Int) =
    if (add.amountMsat < c.localParams.htlcMinimumMsat) throw new LightningException
    else if (add.id != c.remoteNextHtlcId) throw new LightningException
    else if (add.paymentHash.size != 32) throw new LightningException
    else if (add.expiry <= blockLimit) throw new LightningException
    else {

      // Let's compute the current commitment *as seen by us* including this change
      val c1 = addRemoteProposal(c, add).copy(remoteNextHtlcId = c.remoteNextHtlcId + 1)
      val reduced = CommitmentSpec.reduce(c1.localCommit.spec, c1.localChanges.acked, c1.remoteChanges.proposed)
      val fees = if (c1.localParams.isFunder) Scripts.commitTxFee(Satoshi(c1.localParams.dustLimitSatoshis), reduced).amount else 0
      val htlcValueInFlightOverflow = reduced.htlcs.map(_.add.amountMsat).sum > c1.localParams.maxHtlcValueInFlightMsat
      val feesOverflow = reduced.toRemoteMsat / satFactor - c1.localParams.channelReserveSat - fees < 0
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.localParams.maxAcceptedHtlcs

      // The rest of the guards
      if (htlcValueInFlightOverflow) throw new LightningException
      else if (acceptedHtlcsOverflow) throw new LightningException
      else if (feesOverflow) throw new LightningException
      else c1
    }

  def sendFulfill(c: Commitments, cmd: CMDFulfillHtlc) = {
    val fulfill = UpdateFulfillHtlc(c.channelId, cmd.id, cmd.preimage)
    getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id) match {
      case Some(add) if fulfill.paymentHash == add.paymentHash =>
        addLocalProposal(c, fulfill) -> fulfill

      case Some(add) => throw new LightningException
      case _ => throw new LightningException
    }
  }

  private def receiveFulfill(c: Commitments, fulfill: UpdateFulfillHtlc) =
    getHtlcCrossSigned(c, incomingRelativeToLocal = false, fulfill.id) match {
      case Some(add) if fulfill.paymentHash == add.paymentHash =>
        addRemoteProposal(c, fulfill)

      case Some(add) => throw new LightningException
      case _ => throw new LightningException
    }

  def sendFail(c: Commitments, cmd: CMDFailHtlc) = {
    val fail = UpdateFailHtlc(c.channelId, cmd.id, cmd.reason)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id)
    if (found.isEmpty) throw new LightningException else addLocalProposal(c, fail) -> fail
  }

  def sendFailMalformed(c: Commitments, cmd: CMDFailMalformedHtlc) = {
    val fail = UpdateFailMalformedHtlc(c.channelId, cmd.id, cmd.onionHash, cmd.code)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, htlcId = cmd.id)
    if (found.isEmpty) throw new LightningException else addLocalProposal(c, fail) -> fail
  }

  private def receiveFail(c: Commitments, fail: FailHtlc) = {
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id)
    if (found.isEmpty) throw new LightningException else addRemoteProposal(c, fail)
  }

  def sendFee(c: Commitments, ratePerKw: Long) = {
    val updateFee = UpdateFee(c.channelId, ratePerKw)
    val c1 = addLocalProposal(c, updateFee)

    val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
    val fees = Scripts.commitTxFee(dustLimit = Satoshi(c1.remoteParams.dustLimitSatoshis), spec = reduced).amount
    val feesOverflow = reduced.toRemoteMsat / satFactor - c1.remoteParams.channelReserveSatoshis - fees < 0
    if (feesOverflow) throw new LightningException else c1 -> updateFee
  }

  def sendCommit(c: Commitments, remoteNextPerCommitmentPoint: Point) = {
    // Remote commitment will include all local proposed changes as well as remote acked changes
    val spec = CommitmentSpec.reduce(c.remoteCommit.spec, c.remoteChanges.acked, c.localChanges.proposed)
    val paymentKey = Generators.derivePrivKey(c.localParams.paymentKey, remoteNextPerCommitmentPoint)

    val (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      Helpers.makeRemoteTxs(c.remoteCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, remoteNextPerCommitmentPoint, spec)

    // Generate signatures
    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, paymentKey)

    // Update commitment data
    val commitSig = CommitSig(c.channelId, Scripts.sign(remoteCommitTx, c.localParams.fundingPrivKey), htlcSigs.toList)
    val remote1 = RemoteCommit(c.remoteCommit.index + 1, spec, remoteCommitTx.tx.txid, remoteNextPerCommitmentPoint)
    val localChanges1 = c.localChanges.copy(proposed = Vector.empty, signed = c.localChanges.proposed)
    val remoteChanges1 = c.remoteChanges.copy(acked = Vector.empty, signed = c.remoteChanges.acked)
    val c1 = c.copy(remoteNextCommitInfo = Left apply WaitingForRevocation(remote1, commitSig),
      localChanges = localChanges1, remoteChanges = remoteChanges1)

    c1 -> commitSig
  }

  def receiveCommit(c: Commitments, commit: CommitSig) = {
    val spec = CommitmentSpec.reduce(c.localCommit.spec, c.localChanges.acked, c.remoteChanges.proposed)
    val localPerCommitmentSecret = Generators.perCommitSecret(c.localParams.shaSeed, c.localCommit.index)
    val localPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 1)
    val localNextPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 2)
    val remotePaymentPubkey = Generators.derivePubKey(c.remoteParams.paymentBasepoint, localPerCommitmentPoint)
    val localPaymentKey = Generators.derivePrivKey(c.localParams.paymentKey, localPerCommitmentPoint)
    val revocation = RevokeAndAck(c.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)

    val (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      Helpers.makeLocalTxs(c.localCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, localPerCommitmentPoint, spec)

    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val signedCommitTx = Scripts.addSigs(localCommitTx, c.localParams.fundingPrivKey.publicKey,
      c.remoteParams.fundingPubKey, Scripts.sign(localCommitTx, c.localParams.fundingPrivKey),
      remoteSig = commit.signature)

    if (Scripts.checkSpendable(signedCommitTx).isFailure) throw new LightningException
    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw new LightningException

    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, localPaymentKey)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    val htlcTxsAndSigs = combined.collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val check = Scripts checkSpendable Scripts.addSigs(htlcTx, localSig, remoteSig)
        if (check.isFailure) throw new LightningException else HtlcTxAndSigs(htlcTx, localSig, remoteSig)

      case (htlcTx: HtlcSuccessTx, localSig, remoteSig) =>
        val isSigValid = Scripts.checkSig(htlcTx, remoteSig, remotePaymentPubkey)
        if (!isSigValid) throw new LightningException else HtlcTxAndSigs(htlcTx, localSig, remoteSig)
    }

    val localChanges1 = c.localChanges.copy(acked = Vector.empty)
    val localCommit1 = LocalCommit(c.localCommit.index + 1, spec, htlcTxsAndSigs, signedCommitTx)
    val remoteChanges1 = c.remoteChanges.copy(proposed = Vector.empty, acked = c.remoteChanges.acked ++ c.remoteChanges.proposed)
    c.copy(remoteChanges = remoteChanges1, localChanges = localChanges1, localCommit = localCommit1) -> revocation
  }

  def receiveRevocation(c: Commitments, rev: RevokeAndAck) = c.remoteNextCommitInfo match {
    case Left(_) if c.remoteCommit.remotePerCommitmentPoint != rev.perCommitmentSecret.toPoint =>
      throw new LightningException

    case Left(wait: WaitingForRevocation) =>
      val nextIndex = ShaChain.largestTxIndex - c.remoteCommit.index
      val secrets1 = ShaChain.addHash(c.remotePerCommitmentSecrets, rev.perCommitmentSecret.toBin, nextIndex)
      val localChanges1 = c.localChanges.copy(signed = Vector.empty, acked = c.localChanges.acked ++ c.localChanges.signed)

      c.copy(localChanges = localChanges1, remoteChanges = c.remoteChanges.copy(signed = Vector.empty),
        remoteCommit = wait.nextRemoteCommit, remoteNextCommitInfo = Right apply rev.nextPerCommitmentPoint,
        remotePerCommitmentSecrets = secrets1)

    // Unexpected revocation when we have Point
    case _ => throw new LightningException
  }
}