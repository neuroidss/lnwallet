package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.wallet.lncloud.{CipherOpenHelper, LocalBroadcaster}
import com.lightning.wallet.Utils.app


object LNParams {
  var nodeId: PublicKey = _
  var extendedPrivateKey: ExtendedPrivateKey = _
  var extendedCloudPrivateKey: ExtendedPrivateKey = _
  var broadcaster: Broadcaster = _
  var db: CipherOpenHelper = _

  def isSetUp: Boolean = db != null
  def setup(seed: BinaryData): Unit = generate(seed) match { case master =>
    extendedPrivateKey = derivePrivateKey(master, hardened(46) :: hardened(0) :: Nil)
    extendedCloudPrivateKey = derivePrivateKey(master, hardened(92) :: hardened(0) :: Nil)
    db = new CipherOpenHelper(app, 1, Crypto.hash256(seed).toString)
    nodeId = extendedPrivateKey.publicKey
    broadcaster = LocalBroadcaster
  }

  val updateFeeMinDiffRatio = 0.25 // Must update
  val maxChannelCapacity = MilliSatoshi(16777216000L)
  val maxHtlcValue = MilliSatoshi(4294967295L)
  val maxReserveToFundingRatio = 0.05 // %
  val reserveToFundingRatio = 0.01 // %
  val untilExpiryBlocks = 6
  val minDepth = 2

  def deriveParamsPrivateKey(index: Long, n: Long): PrivateKey =
    derivePrivateKey(extendedPrivateKey, index :: n :: Nil).privateKey

  def myHtlcExpiry = broadcaster.currentHeight + untilExpiryBlocks
  def exceedsReserve(channelReserveSatoshis: Long, fundingSatoshis: Long): Boolean =
    channelReserveSatoshis.toDouble / fundingSatoshis > maxReserveToFundingRatio

  def shouldUpdateFee(commitmentFeeratePerKw: Long, networkFeeratePerKw: Long): Boolean = {
    val feeRatio = (networkFeeratePerKw - commitmentFeeratePerKw) / commitmentFeeratePerKw.toDouble
    networkFeeratePerKw > 0 && Math.abs(feeRatio) > updateFeeMinDiffRatio
  }

  def makeLocalParams(fundingSat: Long, finalScriptPubKey: BinaryData, keyIndex: Long) =
    LocalParams(Block.TestnetGenesisBlock.blockId, dustLimitSatoshis = 542, maxHtlcValueInFlightMsat = Long.MaxValue,
      channelReserveSatoshis = (reserveToFundingRatio * fundingSat).toLong, htlcMinimumMsat = 500, toSelfDelay = 144, maxAcceptedHtlcs = 10,
      fundingPrivKey = deriveParamsPrivateKey(keyIndex, 0L), revocationSecret = deriveParamsPrivateKey(keyIndex, 1L), paymentKey = deriveParamsPrivateKey(keyIndex, 2L),
      delayedPaymentKey = deriveParamsPrivateKey(keyIndex, 3L), defaultFinalScriptPubKey = finalScriptPubKey, shaSeed = sha256(deriveParamsPrivateKey(keyIndex, 4L).toBin),
      isFunder = true, globalFeatures = "", localFeatures = "01")
}

trait Broadcaster {
  def broadcast(tx: Transaction)
  def currentFeeRate: Long
  def currentHeight: Int

  def broadcastStatus(txs: Seq[Transaction], parents: Map[String, Int], chainHeight: Int): Seq[BroadcastStatus] = {
    val augmented = for (tx <- txs) yield (tx, parents get tx.txIn.head.outPoint.txid.toString, Scripts csvTimeout tx)

    augmented map {
      // If CSV is zero then whether parent tx is present or not is irrelevant, we look as CLTV
      case (tx, _, 0L) if tx.lockTime - chainHeight < 1 => BroadcastStatus(None, publishable = true, tx)
      case (tx, _, 0L) => BroadcastStatus(Some(tx.lockTime - chainHeight), publishable = false, tx)
      // If CSV is not zero but parent tx is not published then we wait for parent
      case (tx, None, _) => BroadcastStatus(None, publishable = false, tx)

      case (tx, Some(parentConfs), csv) =>
        // Tx may have both CLTV and CSV so we need to get the max of them
        val blocksLeft = math.max(csv - parentConfs, tx.lockTime - chainHeight)
        if (blocksLeft < 1) BroadcastStatus(None, publishable = true, tx)
        else BroadcastStatus(Some(blocksLeft), publishable = false, tx)
    }
  }
}