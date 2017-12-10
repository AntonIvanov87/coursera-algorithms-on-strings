package A2

import common.BaseCodec

private object A2_1_BWT {

  def bwt(text: String): String = {
    val startIndices = new Array[Int](text.length)
    for (i <- 0 until text.length) {
      startIndices(i) = i
    }

    val bytes = BaseCodec.toBytes(text)
    val indexToByte: (Int) => Byte = index => {
      bytes(index % bytes.length)
    }
//    StartIndices.sortMSD(startIndices, indexToByte)
     StartIndices.sortQuick3Way(startIndices, indexToByte)

    val bwtChars = for (i <- startIndices) yield {
      val prevI = if (i > 0) i - 1 else text.length - 1
      text.charAt(prevI)
    }
    String.valueOf(bwtChars)
  }



}