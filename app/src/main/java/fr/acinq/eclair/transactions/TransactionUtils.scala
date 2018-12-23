package fr.acinq.eclair.transactions


object TransactionUtils {
  import fr.acinq.bitcoin._

  case class CltvOptTxOut(cltvOpt: Option[Long], txOut: TxOut)
  def sort(tx: Transaction): Transaction = LexicographicalOrdering.sort(tx)

  private def isLessThan(input1: TxIn, input2: TxIn): Boolean =
    isLessThan(input1.outPoint, input2.outPoint)

  private def isLessThan(output1: OutPoint, output2: OutPoint): Boolean =
    if (output1.txid == output2.txid) output1.index < output2.index
    else lexicographicalOrder(output1.txid, output2.txid) < 0

  private def isLessOrCLTV(a: CltvOptTxOut, b: CltvOptTxOut) = {
    val amountComparison = a.txOut.amount.compare(b.txOut.amount)

    if (amountComparison != 0) amountComparison < 0 else {
      val lexGraphComparsion = lexicographicalOrder(a.txOut.publicKeyScript, b.txOut.publicKeyScript)
      if (lexGraphComparsion == 0 && a.cltvOpt.isDefined && b.cltvOpt.isDefined) a.cltvOpt.get < b.cltvOpt.get
      else lexGraphComparsion < 0
    }
  }

  type SeqByte = Seq[Byte]
  private def lexicographicalOrder(a: SeqByte, b: SeqByte): Int =
    if (a.isEmpty && b.isEmpty) 0 else if (a.isEmpty) 1 else if (b.isEmpty) -1
    else if (a.head == b.head) lexicographicalOrder(a.tail, b.tail)
    else (a.head & 0xff).compareTo(b.head & 0xff)

  type TxOutCltvMap = Map[TxOut, Long]
  def sortByBIP69AndCLTV(tx: Transaction, offeredHtlcAndCltv: TxOutCltvMap): Transaction = {
    // Transaction outputs with optionally a CLTV value attached, only outputs corresponding to offered HTLCs will have it
    val tempMap = for (Tuple2(txOut, cltv) <- offeredHtlcAndCltv) yield txOut.publicKeyScript -> CltvOptTxOut(Some(cltv), txOut)
    val cltvOptTxOuts = for (txOut <- tx.txOut) yield tempMap.getOrElse(default = CltvOptTxOut(None, txOut), key = txOut.publicKeyScript)
    val sortedTxOuts = for (cltvOptTxOut <- cltvOptTxOuts sortWith isLessOrCLTV) yield cltvOptTxOut.txOut
    tx.copy(txIn = tx.txIn sortWith isLessThan, txOut = sortedTxOuts)
  }
}