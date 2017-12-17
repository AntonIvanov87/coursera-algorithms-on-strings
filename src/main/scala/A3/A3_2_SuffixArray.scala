package A3

import common.BaseCodec

private object A3_2_SuffixArray {

  def sortStartIndices(text: String): Array[Int] = {
    var startIndices = sortStartIndices1(text)

    var startIToClass = new Array[Int](startIndices.length)
    startIToClass(startIndices(0)) = 0
    for (i <- 1 until startIndices.length) {
      val startI = startIndices(i)
      val prevStartI = startIndices(i - 1)
      startIToClass(startI) = startIToClass(prevStartI)
      if (text.charAt(startI) != text.charAt(prevStartI)) {
        startIToClass(startI) += 1
      }
    }

    var suffixLen = 1
    while (suffixLen < text.length) {
      val startIndicesDoubled = sortStartIndicesDoubled(startIndices, suffixLen, startIToClass)
      if (java.util.Arrays.equals(startIndices, startIndicesDoubled)) {
        return startIndices
      }

      startIndices = startIndicesDoubled
      startIToClass = updateStartIToClass(startIToClass, startIndices, suffixLen)
      suffixLen *= 2
    }

    startIndices
  }

  private def sortStartIndices1(text: String): Array[Int] = {
    val byteToNextToEndIndex = new Array[Int](5)
    for (ch <- text) {
      val byte = BaseCodec.toByte(ch)
      byteToNextToEndIndex(byte) += 1
    }
    for (i <- 1 until byteToNextToEndIndex.length) {
      byteToNextToEndIndex(i) += byteToNextToEndIndex(i - 1)
    }

    val startIndices = new Array[Int](text.length)
    for (startIndex <- Range(text.length - 1, -1, -1)) {
      val ch = text.charAt(startIndex)
      val byte = BaseCodec.toByte(ch)
      byteToNextToEndIndex(byte) -= 1
      val targetIndex = byteToNextToEndIndex(byte)
      startIndices(targetIndex) = startIndex
    }
    startIndices
  }

  private def sortStartIndicesDoubled(startIndices: Array[Int], suffixLen: Int, startIToClass: Array[Int]): Array[Int] = {
    val classToNextToEndIndex = new Array[Int](startIToClass(startIndices.last)+1)
    for (cl <- startIToClass) {
      classToNextToEndIndex(cl) += 1
    }
    for (i <- 1 until classToNextToEndIndex.length) {
      classToNextToEndIndex(i) += classToNextToEndIndex(i - 1)
    }

    val sortedStartIDoubled = new Array[Int](startIndices.length)
    for (i <- Range(startIndices.length - 1, -1, -1)) {
      val doubledStartI = (startIndices(i) - suffixLen + startIndices.length) % startIndices.length
      val doubledCl = startIToClass(doubledStartI)
      classToNextToEndIndex(doubledCl) -= 1
      val targetI = classToNextToEndIndex(doubledCl)
      sortedStartIDoubled(targetI) = doubledStartI
    }
    sortedStartIDoubled
  }

  def updateStartIToClass(prevStartIToClass: Array[Int], startIndices: Array[Int], suffixLen: Int): Array[Int] = {
    val updatedStartIToClass = new Array[Int](prevStartIToClass.length)
    updatedStartIToClass(startIndices(0)) = 0
    for (i <- 1 until startIndices.length) {
      val curStartI = startIndices(i)
      val prevStartI = startIndices(i-1)

      updatedStartIToClass(curStartI) = updatedStartIToClass(prevStartI)
      if (prevStartIToClass(curStartI) == prevStartIToClass(prevStartI)) {
        val curStartI2 = (curStartI + suffixLen) % startIndices.length
        val prevStartI2 = (prevStartI + suffixLen) % startIndices.length
        if (prevStartIToClass(curStartI2) != prevStartIToClass(prevStartI2)) {
          updatedStartIToClass(curStartI) += 1
        }
      } else {
        updatedStartIToClass(curStartI) += 1
      }
    }

    updatedStartIToClass
  }

}
