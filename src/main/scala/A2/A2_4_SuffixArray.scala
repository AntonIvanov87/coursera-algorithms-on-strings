package A2

import common.BaseCodec

private object A2_4_SuffixArray {

  def sortedStartIndices(text: String): Array[Int] = {
    val startIndices = new Array[Int](text.length)
    for (i <- 0 until text.length) {
      startIndices(i) = i
    }

    val bytes = BaseCodec.toBytes(text)
    val indexToByte: (Int) => Byte = index => {
      if (index >= bytes.length) {
        -1
      } else {
        bytes(index)
      }
    }

    StartIndices.sortMSD(startIndices, indexToByte)

    startIndices
  }

}
