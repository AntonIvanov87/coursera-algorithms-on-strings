package A3

import scala.collection.mutable.ArrayBuffer

private object A3_1_KMP {

  def find(what: String, where: String): Seq[Int] = {
    val indexToBorderLen = computeIndexToBorderLen(what + '$' + where)
    val indicesOfWhat = ArrayBuffer[Int]()
    for (i <- indexToBorderLen.indices) {
      if (indexToBorderLen(i) == what.length) {
        indicesOfWhat += i - 2 * what.length
      }
    }
    indicesOfWhat
  }

  private def computeIndexToBorderLen(str: String): Array[Int] = {
    val indexToBorderLen = new Array[Int](str.length)
    indexToBorderLen(0) = 0
    for (i <- 1 until str.length) {
      indexToBorderLen(i) = findBorderLen(str, i, indexToBorderLen)
    }
    indexToBorderLen
  }

  private def findBorderLen(str: String, i: Int, indexToBorderLen: Array[Int]): Int = {
    var borderLen = indexToBorderLen(i-1)
    while(true) {
      if (str.charAt(i) == str.charAt(borderLen)) {
        return borderLen + 1
      }
      if (borderLen == 0) {
        return 0
      }
      borderLen = indexToBorderLen(borderLen-1)
    }
    throw new IllegalStateException
  }

}
