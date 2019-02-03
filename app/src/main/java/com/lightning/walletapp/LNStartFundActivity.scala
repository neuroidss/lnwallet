package com.lightning.walletapp

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.ChannelUploadAct
import com.lightning.walletapp.ln.Scripts.pubKeyScript
import com.lightning.walletapp.helper.AES
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.wallet.SendRequest
import fr.acinq.bitcoin.MilliSatoshi
import android.app.AlertDialog
import org.bitcoinj.core.Batch
import android.os.Bundle

import android.widget.{ImageButton, TextView}
import scala.util.{Success, Try}


class LNStartFundActivity extends TimerActivity { me =>
  lazy val lnStartFundCancel = findViewById(R.id.lnStartFundCancel).asInstanceOf[ImageButton]
  lazy val lnStartFundDetails = findViewById(R.id.lnStartFundDetails).asInstanceOf[TextView]
  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  override def onBackPressed = whenBackPressed.run

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_start_fund)
    val fundNodeView = app getString ln_ops_start_fund_node_view

    app.TransData checkAndMaybeErase {
      case remoteNodeView @ RemoteNodeView(ann \ _) => proceed(None, remoteNodeView.asString(fundNodeView), ann)
      case hardcodedNodeView @ HardcodedNodeView(ann, _) => proceed(None, hardcodedNodeView.asString(fundNodeView), ann)
      case ann: NodeAnnouncement => proceed(None, HardcodedNodeView(ann, tip = "( ͡° ͜ʖ ͡°)").asString(fundNodeView), ann)
      case icp: IncomingChannelParams => proceed(Some(icp), icp.nodeView.asString(fundNodeView), icp.nodeView.ann)
      case _ => finish
    }

    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]

  def proceed(icrOpt: Option[IncomingChannelParams], asString: String, ann: NodeAnnouncement) = {
    val freshChan = ChannelManager.createChannel(bootstrap = InitData(ann), initialListeners = Set.empty)
    val peerIncompatible = new LightningException(me getString err_ln_peer_incompatible format ann.alias)
    val peerOffline = new LightningException(me getString err_ln_peer_offline format ann.alias)
    lnStartFundCancel setOnClickListener onButtonTap(whenBackPressed.run)
    lnStartFundDetails setText asString.html

    class OpenListener extends ConnectionListener with ChannelListener {
      override def onTerminalError(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> peerOffline)
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> peerOffline)

      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case open: OpenChannel if nodeId == ann.nodeId && open.channelFlags == 0.toByte => onOpenOffer(nodeId, open)
        case remoteError: Error if nodeId == ann.nodeId => onException(freshChan -> remoteError.exception)
        case _: ChannelSetupMessage if nodeId == ann.nodeId => freshChan process msg
        case _ => // We only listen to setup messages here to avoid conflicts
      }

      override def onOpenOffer(nodeId: PublicKey, open: OpenChannel) =
        if (System.currentTimeMillis > ConnectionManager.lastOpenChannelOffer + 7500L) {
          val hnv = HardcodedNodeView(ann, app getString ln_ops_start_fund_incoming_channel)
          ConnectionManager.lastOpenChannelOffer = System.currentTimeMillis
          app.TransData.value = IncomingChannelParams(hnv, open)
          runAnd(finish)(me startActivity getIntent)
        }

      override def onException = {
        case _ \ errorWhileOpening =>
          // Inform user, disconnect this channel, go back
          UITask(app toast errorWhileOpening.getMessage).run
          whenBackPressed.run
      }
    }

    abstract class LocalOpenListener extends OpenListener {
      override def onOperational(nodeId: PublicKey, isCompat: Boolean) = if (nodeId == ann.nodeId) {
        // Peer has sent us their Init so we ask user to provide a funding amount if peer is compatible
        if (isCompat) askLocalFundingConfirm.run else onException(freshChan -> peerIncompatible)
      }

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we create a real funding transaction
          // We create a funding transaction by replacing an output with a real one in a saved dummy funding transaction
          val req = cmd.batch replaceDummy pubKeyScript(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
          freshChan process CMDFunding(app.kit.sign(req).tx)

        case (_, wait: WaitFundingDoneData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, save channel and broadcast a fund tx
          freshChan.listeners = ChannelManager.operationalListeners
          ChannelManager.all +:= freshChan
          saveChan(wait)

          // Broadcast a funding transaction
          // Tell wallet activity to redirect to ops
          LNParams.broadcaster nullOnBecome freshChan
          app.TransData.value = FragWallet.REDIRECT
          me exitTo MainActivity.wallet
      }

      // Provide manual or batched amount
      def askLocalFundingConfirm: Runnable
    }

    def saveChan(some: HasCommitments) = {
      // First of all we should store this chan
      // error here will halt all further progress
      freshChan STORE some

      app.kit.wallet.addWatchedScripts(app.kit fundingPubScript some)
      // Start watching a channel funding script and save a channel, order an encrypted backup upload
      val encrypted = AES.encReadable(RefundingData(some.announce, None, some.commitments).toJson.toString, LNParams.cloudSecret)
      val chanUpload = ChannelUploadAct(encrypted.toHex, Seq("key" -> LNParams.cloudId.toString), "data/put", some.announce.alias)
      app.olympus.tellClouds(chanUpload)
    }

    def localWalletListener = new LocalOpenListener {
      // Asks user to provide a funding amount manually

      def askLocalFundingConfirm = UITask {
        val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
        val maxCap = MilliSatoshi(math.min(app.kit.conf0Balance.value, LNParams.maxCapacity.amount) * 1000L)
        val minCap = MilliSatoshi(math.max(LNParams.broadcaster.perKwThreeSat * 3, LNParams.minCapacitySat) * 1000L)
        val rateManager = new RateManager(content) hint getString(amount_hint_newchan).format(denom parsedWithSign minCap,
          denom parsedWithSign LNParams.maxCapacity, denom parsedWithSign app.kit.conf0Balance)

        def askAttempt(alert: AlertDialog) = rateManager.result match {
          case Success(ms) if ms < minCap => app toast dialog_sum_small
          case Success(ms) if ms > maxCap => app toast dialog_sum_big

          case Success(ms) =>
            val txProcessor = new TxProcessor {
              val dummyKey = randomPrivKey.publicKey
              val dummyScript = pubKeyScript(dummyKey, dummyKey)
              val pay = P2WSHData(ms, pay2wsh = dummyScript)

              def futureProcess(unsigned: SendRequest) = {
                val fee = LNParams.broadcaster.perKwThreeSat
                val batch = Batch(unsigned, dummyScript, null)
                val theirReserveSat = batch.fundingAmountSat / LNParams.channelReserveToFundingRatio
                val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
                val localParams = LNParams.makeLocalParams(ann, theirReserveSat, finalPubKeyScript, System.currentTimeMillis, isFunder = true)
                freshChan process CMDOpenChannel(localParams, tempChanId = random getBytes 32, fee, batch, batch.fundingAmountSat, pushMsat = 0L)
              }

              def onTxFail(err: Throwable) =
                mkCheckForm(alert => rm(alert)(finish), none,
                  baseBuilder(txMakeError(err), null), dialog_ok, -1)
            }

            val coloredAmount = denom.coloredP2WSH(txProcessor.pay.cn, denom.sign)
            val coloredExplanation = txProcessor.pay destination coloredAmount
            rm(alert)(txProcessor start coloredExplanation)

          case _ =>
            app toast dialog_sum_small
        }

        def useMax(alert: AlertDialog) = rateManager setSum Try(maxCap)
        val bld = baseBuilder(getString(ln_ops_start_fund_local_title).html, content)
        mkCheckFormNeutral(askAttempt, none, useMax, bld, dialog_next, dialog_cancel, dialog_max)
      }
    }

    def localBatchListener(batch: Batch) = new LocalOpenListener {
      // Uses a prebuilt batch which funds a channel and pays onchain

      def askLocalFundingConfirm = UITask {
        val text = batch asString ln_open_batch_confirm
        val title = getString(ln_ops_start_fund_local_title).html

        mkCheckForm(alert => rm(alert) {
          val fee = LNParams.broadcaster.perKwThreeSat
          val theirReserveSat = batch.fundingAmountSat / LNParams.channelReserveToFundingRatio
          val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
          val localParams = LNParams.makeLocalParams(ann, theirReserveSat, finalPubKeyScript, System.currentTimeMillis, isFunder = true)
          freshChan process CMDOpenChannel(localParams, tempChanId = random getBytes 32, fee, batch, batch.fundingAmountSat, pushMsat = 0L)
        }, none, baseBuilder(title, text), dialog_next, dialog_cancel)
      }
    }

    def remoteOpenFundeeListener(open: OpenChannel) = new OpenListener {
      // In this special case we already have their OpenChannel and connection is active
      freshChan process Tuple2(LNParams.makeLocalParams(ann, open.fundingSatoshis / LNParams.channelReserveToFundingRatio,
        ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram, System.currentTimeMillis, isFunder = false), open)

      override def onBecome = {
        case (_, wait: WaitBroadcastRemoteData, WAIT_FOR_FUNDING, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, save channel and wait for their tx
          freshChan.listeners = ChannelManager.operationalListeners
          ChannelManager.all +:= freshChan
          saveChan(wait)

          // Tell wallet activity to redirect to ops
          app.TransData.value = FragWallet.REDIRECT
          me exitTo MainActivity.wallet
      }
    }

    lazy val openListener = FragLNStart.batchOpt -> icrOpt match {
      case None \ Some(inChan) => remoteOpenFundeeListener(inChan.open)
      case Some(batch) \ None => localBatchListener(batch)
      case _ => localWalletListener
    }

    whenBackPressed = UITask {
      freshChan.listeners -= openListener
      ConnectionManager.listeners -= openListener
      // Worker may have already been removed on some connection failure
      ConnectionManager.connections.get(ann.nodeId).foreach(_.disconnect)
      finish
    }

    freshChan.listeners += openListener
    ConnectionManager.listeners += openListener
    ConnectionManager.connectTo(ann, notify = true)
  }
}