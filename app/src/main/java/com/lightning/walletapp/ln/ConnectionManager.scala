package com.lightning.walletapp.ln

import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.ln.Features._

import rx.lang.scala.{Observable => Obs}
import java.net.{InetSocketAddress, Socket}
import com.lightning.walletapp.ln.Tools.{Bytes, none}
import com.lightning.walletapp.ln.crypto.Noise.KeyPair
import java.util.concurrent.ConcurrentHashMap
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.tor.OnionAddress
import java.util.concurrent.Executors
import fr.acinq.bitcoin.BinaryData


object ConnectionManager {
  var lastOpenChannelOffer = 0L
  var listeners = Set.empty[ConnectionListener]
  val connections = new ConcurrentHashMap[PublicKey, Worker].asScala

  protected[this] val events = new ConnectionListener {
    override def onMessage(nodeId: PublicKey, msg: LightningMessage) = for (lst <- listeners) lst.onMessage(nodeId, msg)
    override def onOperational(nodeId: PublicKey, isCompat: Boolean) = for (lst <- listeners) lst.onOperational(nodeId, isCompat)
    override def onTerminalError(nodeId: PublicKey) = for (lst <- listeners) lst.onTerminalError(nodeId)
    override def onDisconnect(nodeId: PublicKey) = for (lst <- listeners) lst.onDisconnect(nodeId)
  }

  def connectTo(ann: NodeAnnouncement, notify: Boolean) = connections get ann.nodeId match {
    case Some(workerIsPresent) => if (notify) events.onOperational(ann.nodeId, isCompat = true)
    case None => connections += ann.nodeId -> new Worker(ann)
  }

  class Worker(val ann: NodeAnnouncement) {
    implicit val context = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
    private val keyPair = KeyPair(nodePublicKey.toBin, nodePrivateKey.toBin)
    private val buffer = new Bytes(1024)
    val socket = new Socket

    val handler: TransportHandler = new TransportHandler(keyPair, ann.nodeId) {
      def handleEnterOperationalState = handler process Init(LNParams.globalFeatures, LNParams.localFeatures)
      def handleDecryptedIncomingData(data: BinaryData) = intercept(LightningMessageCodecs deserialize data)
      def handleEncryptedOutgoingData(data: BinaryData) = try socket.getOutputStream write data catch none
      def handleError = { case _ => events onTerminalError ann.nodeId }
    }

    val work = Future {
      socket.connect(ann.addresses.collectFirst {
        case IPv4(sockAddress, port) => new InetSocketAddress(sockAddress, port)
        case IPv6(sockAddress, port) => new InetSocketAddress(sockAddress, port)
        case Tor2(address, port) => OnionAddress.fromParts(address, port).toInetSocketAddress
        case Tor3(address, port) => OnionAddress.fromParts(address, port).toInetSocketAddress
      }.get, 7500)

      handler.init
      while (true) {
        val length = socket.getInputStream.read(buffer, 0, buffer.length)
        if (length < 0) throw new RuntimeException("Connection droppped")
        else handler process BinaryData(buffer take length)
      }
    }

    work onComplete { _ =>
      connections -= ann.nodeId
      events onDisconnect ann.nodeId
    }

    var lastMsg = System.currentTimeMillis
    def disconnect = try socket.close catch none
    def intercept(message: LightningMessage) = {
      // Update liveness on each incoming message
      lastMsg = System.currentTimeMillis

      message match {
        case their: Init =>
          val dlp = dataLossProtect(their.localFeatures)
          val baseCompat = areSupported(their.localFeatures)
          events.onOperational(ann.nodeId, dlp && baseCompat)

        case Ping(len, _) if len > 0 && len <= 65532 => handler process Pong("00" * len)
        case internalMessage => events.onMessage(ann.nodeId, internalMessage)
      }
    }
  }

  for {
    _ <- Obs interval 30.seconds
    outdated = System.currentTimeMillis - 1000L * 60
    _ \ work <- connections if work.lastMsg < outdated
  } work.disconnect
}

class ConnectionListener {
  def onOpenOffer(nodeId: PublicKey, msg: OpenChannel): Unit = none
  def onMessage(nodeId: PublicKey, msg: LightningMessage): Unit = none
  def onOperational(nodeId: PublicKey, isCompat: Boolean): Unit = none
  def onTerminalError(nodeId: PublicKey): Unit = none
  def onDisconnect(nodeId: PublicKey): Unit = none
}