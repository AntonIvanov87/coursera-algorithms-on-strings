package A2

import java.util
import java.util.Comparator

object A2_1_BWT {

  def bwt(text: String): String = {
    val startIndices = new Array[Integer](text.length)
    for (i <- 0 until text.length) {
      startIndices(i) = i
    }

    util.Arrays.sort(startIndices, new Comparator[Integer] {
      override def compare(i1: Integer, i2: Integer): Int = {
        var offset = 0
        while (offset < startIndices.length) {
          val ch1 = text.charAt((i1 + offset) % startIndices.length)
          val ch2 = text.charAt((i2 + offset) % startIndices.length)
          if (ch1 < ch2) {
            return -1
          } else if (ch1 > ch2) {
            return 1
          }
          offset+=1
        }
        0
      }
    })

    val bwtChars = for (i <- startIndices) yield {
      val prevI = if (i > 0) i - 1 else text.length - 1
      text(prevI)
    }
    String.valueOf(bwtChars)
  }

}
