package A2

import common.BaseCodec

object A2_1_BWT {

  def bwt(text: String): String = {
    val startIndices = new Array[Int](text.length)
    for (i <- 0 until text.length) {
      startIndices(i) = i
    }

    sortStartIndicesMSD(startIndices, text)
//    sortStartIndicesQuick3Way(startIndices, text)

    val bwtChars = for (i <- startIndices) yield {
      val prevI = if (i > 0) i - 1 else text.length - 1
      text.charAt(prevI)
    }
    String.valueOf(bwtChars)
  }

  private def sortStartIndicesMSD(startIndices: Array[Int], text: String): Unit = {
    val bytes = BaseCodec.toBytes(text)
    sortStartIndicesMSD(startIndices, bytes, 0, bytes.length - 1, 0)
  }

  private def sortStartIndicesMSD(startIndices: Array[Int],
                                  bytes: Array[Byte],
                                  fromIndex: Int,
                                  toIndex: Int,
                                  startIndexOffset: Int): Unit = {

    if (fromIndex == toIndex) {
      return
    }

    val byteToCount = new Array[Int](5)
    for (i <- fromIndex to toIndex) {
      val byteIndex = (startIndices(i) + startIndexOffset) % bytes.length
      val byte = bytes(byteIndex)
      byteToCount(byte) += 1
    }

    val byteToIndex = new Array[Int](5)
    for (i <- 1 until byteToIndex.length) {
      byteToIndex(i) = byteToIndex(i - 1) + byteToCount(i - 1)
    }

    val sortedStartIndices = new Array[Int](toIndex - fromIndex + 1)
    for (i <- sortedStartIndices.indices) {
      val byteIndex = (startIndices(fromIndex + i) + startIndexOffset) % bytes.length
      val byte = bytes(byteIndex)
      val targetIndex = byteToIndex(byte)
      sortedStartIndices(targetIndex) = startIndices(fromIndex + i)
      byteToIndex(byte) += 1
    }

    for (i <- sortedStartIndices.indices) {
      startIndices(fromIndex + i) = sortedStartIndices(i)
    }

    for (i <- byteToIndex.indices) {
      if (byteToCount(i) != 0) {
        val subFromIndex = fromIndex + byteToIndex(i) - byteToCount(i)
        val subToIndex = subFromIndex + byteToCount(i) - 1
        sortStartIndicesMSD(startIndices, bytes, subFromIndex, subToIndex, startIndexOffset + 1)
      }
    }
  }

  private def sortStartIndicesQuick3Way(startIndices: Array[Int], text: String): Unit = {
    val bytes = BaseCodec.toBytes(text)
    sortStartIndicesQuick3Way(startIndices, bytes, 0, startIndices.length - 1, 0)
  }

  private def sortStartIndicesQuick3Way(startIndices: Array[Int], bytes: Array[Byte], fromIndex: Int, toIndex: Int, startIndexOffset: Int): Unit = {
    if (fromIndex < 0 || toIndex >= bytes.length || fromIndex >= toIndex || startIndexOffset >= bytes.length) {
      return
    }

    val baseByteIndex = (startIndices(fromIndex) + startIndexOffset) % bytes.length
    val baseByte = bytes(baseByteIndex)

    var baseStartIndex = fromIndex
    var i = baseStartIndex + 1
    var baseToIndex = toIndex
    while (i <= baseToIndex) {
      val byteIndex = (startIndices(i) + startIndexOffset) % bytes.length
      val byte = bytes(byteIndex)
      if (byte < baseByte) {
        swap(startIndices, baseStartIndex, i)
        baseStartIndex += 1
        i += 1
      } else if (byte > baseByte) {
        swap(startIndices, i, baseToIndex)
        baseToIndex -= 1
      } else {
        i += 1
      }
    }

    sortStartIndicesQuick3Way(startIndices, bytes, fromIndex, baseStartIndex - 1, startIndexOffset)
    sortStartIndicesQuick3Way(startIndices, bytes, baseStartIndex, baseToIndex, startIndexOffset + 1)
    sortStartIndicesQuick3Way(startIndices, bytes, baseToIndex + 1, toIndex, startIndexOffset)
  }

  private def swap(arr: Array[Int], i1: Int, i2: Int): Unit = {
    val temp = arr(i1)
    arr(i1) = arr(i2)
    arr(i2) = temp
  }

}