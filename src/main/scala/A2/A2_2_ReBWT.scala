package A2

import common.BaseCodec

private object A2_2_ReBWT {

  def reBWT(bwtText: String): String = {

    val bwtIndexToCharIndex: Array[Int] = getArrayIndexToCharIndex(bwtText)

    val sortedCol = SortedColumn(bwtText)

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
