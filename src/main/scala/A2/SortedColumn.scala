package A2

import common.BaseCodec

class SortedColumn private (private val byteToFirstIndex: Array[Integer]) {

  def apply(i: Int): Char = {
    var prevByte: Byte = 0
    for (byte <- byteToFirstIndex.indices) {
      val firstIndex = byteToFirstIndex(byte)
      if (firstIndex != null) {
        if (firstIndex > i) {
          return BaseCodec.toBase(prevByte)
        }
        prevByte = byte.asInstanceOf[Byte]
      }
    }
    BaseCodec.toBase(prevByte)
  }

  def firstIndexOf(base: Char): Int = {
    val byte = BaseCodec.toByte(base)
    byteToFirstIndex(byte)
  }

}

object SortedColumn {

  def apply(text: String): SortedColumn = {
    val byteToFirstIndex = getByteToFirstIndex(text)
    new SortedColumn(byteToFirstIndex)
  }

  private def getByteToFirstIndex(text: String): Array[Integer] = {
    val byteToCount = getByteToCount(text)

    val byteToFirstIndex = new Array[Integer](5)
    var prevFirstIndex=0
    var prevCount=0
    for (byte <- byteToCount.indices) {
      val curByteCount = byteToCount(byte)
      if (curByteCount != 0) {
        val curFirstIndex = prevFirstIndex + prevCount
        byteToFirstIndex(byte) = curFirstIndex
        prevFirstIndex = curFirstIndex
        prevCount = byteToCount(byte)
      }
    }
    byteToFirstIndex
  }

  private def getByteToCount(text: String): Array[Int] = {
    val byteToNum = new Array[Int](5)
    for (char <- text) {
      val byte = BaseCodec.toByte(char)
      byteToNum(byte) += 1
    }
    byteToNum
  }
}
