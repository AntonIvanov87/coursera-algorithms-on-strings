package A2

import common.BaseCodec

private object A2_2_ReBWT {

  def reBWT(bwtText: String): String = {

    val bwtIndexToCharIndex: Array[Int] = getArrayIndexToCharIndex(bwtText)

    val sortedCol = new SortedColumn(bwtText)

    val resultChars = new Array[Char](bwtText.length)
    var indexInBWT = 0
    for (i <- Range.inclusive(resultChars.length-1, 0, -1)) {
      val sortedColChar = sortedCol(indexInBWT)
      resultChars(i) = sortedColChar

      val bwtChar = bwtText.charAt(indexInBWT)
      val indexInSortedCol = sortedCol.firstIndexOf(bwtChar) + bwtIndexToCharIndex(indexInBWT)
      indexInBWT = indexInSortedCol
    }

    String.valueOf(resultChars)
  }

  private class SortedColumn(bwtText: String) {
    import SortedColumn._

    private val byteToFirstIndex: Array[Integer] = getByteToFirstIndex(bwtText)

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

  private object SortedColumn {

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

  private def getArrayIndexToCharIndex(bwtText: String) = {
    val bwtIndexToCharIndex = new Array[Int](bwtText.length)
    val byteToNumInBWT = new Array[Int](5)
    for (i <- bwtText.indices) {
      val char = bwtText(i)
      val byte = BaseCodec.toByte(char)
      bwtIndexToCharIndex(i) = byteToNumInBWT(byte)
      byteToNumInBWT(byte) += 1
    }
    bwtIndexToCharIndex
  }

}
