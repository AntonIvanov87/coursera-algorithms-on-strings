package A2

private object StartIndices {

  def sortMSD(startIndices: Array[Int], indexToByte: (Int) => Byte): Unit = {
    sortMSD(startIndices, indexToByte, 0, startIndices.length - 1, 0)
  }

  private def sortMSD(startIndices: Array[Int],
                                  indexToByte: (Int) => Byte,
                                  fromIndex: Int,
                                  toIndex: Int,
                                  startIndexOffset: Int): Unit = {

    if (fromIndex == toIndex) {
      return
    }

    var countOfEmptySuffixes = 0
    val byteToCount = new Array[Int](5)
    for (i <- fromIndex to toIndex) {
      val byte = indexToByte(startIndices(i) + startIndexOffset)
      if (byte == -1) {
        countOfEmptySuffixes += 1
      } else {
        byteToCount(byte) += 1
      }
    }

    var emptySuffixIndex = 0
    val byteToIndex = new Array[Int](byteToCount.length)
    byteToIndex(0) = countOfEmptySuffixes
    for (i <- 1 until byteToIndex.length) {
      byteToIndex(i) = byteToIndex(i - 1) + byteToCount(i - 1)
    }

    val sortedStartIndices = new Array[Int](toIndex - fromIndex + 1)
    for (i <- fromIndex to toIndex) {
      val byte = indexToByte(startIndices(i) + startIndexOffset)
      if (byte == -1) {
        sortedStartIndices(emptySuffixIndex) = startIndices(i)
        emptySuffixIndex += 1

      } else {
        val targetIndex = byteToIndex(byte)
        sortedStartIndices(targetIndex) = startIndices(i)
        byteToIndex(byte) += 1
      }
    }

    for (i <- sortedStartIndices.indices) {
      startIndices(fromIndex + i) = sortedStartIndices(i)
    }

    for (i <- byteToIndex.indices) {
      if (byteToCount(i) != 0) {
        val subFromIndex = fromIndex + byteToIndex(i) - byteToCount(i)
        val subToIndex = subFromIndex + byteToCount(i) - 1
        sortMSD(startIndices, indexToByte, subFromIndex, subToIndex, startIndexOffset + 1)
      }
    }
  }

  def sortQuick3Way(startIndices: Array[Int], indexToByte: (Int) => Byte): Unit = {
    sortQuick3Way(startIndices, indexToByte, 0, startIndices.length - 1, 0)
  }

  private def sortQuick3Way(startIndices: Array[Int], indexToByte: (Int) => Byte, fromIndex: Int, toIndex: Int, startIndexOffset: Int): Unit = {
    if (fromIndex < 0 || toIndex >= startIndices.length || fromIndex >= toIndex || startIndexOffset >= startIndices.length) {
      return
    }

    val baseByte = indexToByte(startIndices(fromIndex) + startIndexOffset)

    var baseStartIndex = fromIndex
    var i = baseStartIndex + 1
    var baseToIndex = toIndex
    while (i <= baseToIndex) {
      val byte = indexToByte(startIndices(i) + startIndexOffset)
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

    sortQuick3Way(startIndices, indexToByte, fromIndex, baseStartIndex - 1, startIndexOffset)
    sortQuick3Way(startIndices, indexToByte, baseStartIndex, baseToIndex, startIndexOffset + 1)
    sortQuick3Way(startIndices, indexToByte, baseToIndex + 1, toIndex, startIndexOffset)
  }

  private def swap(arr: Array[Int], i1: Int, i2: Int): Unit = {
    val temp = arr(i1)
    arr(i1) = arr(i2)
    arr(i2) = temp
  }

}
