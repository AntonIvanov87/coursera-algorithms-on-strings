package A2

import common.BaseCodec

private object A2_3_BetterBWMatching {

  def bwMatch(bwtText: String, patterns: Array[String]): Array[Int] = {
    val betterBWMatcher = BetterBWMatcher(bwtText)
    val patternIToCount = new Array[Int](patterns.length)
    for (pi <- patterns.indices) {
      patternIToCount(pi) = betterBWMatcher getCountOfPattern patterns(pi)
    }
    patternIToCount
  }


  private class BetterBWMatcher private (private val byteToCountBeforeIndices: Array[Array[Int]],
                                         private val sortedColumn: SortedColumn) {

    def getCountOfPattern(pattern: String): Int = {
      var startI = 0
      var endI = byteToCountBeforeIndices(0).length-2 // countBeforeIndices is 1 larger than text
      for (i <- Range(pattern.length-1, -1, -1)) {
        val ch = pattern(i)
        val firstIndexOfChInSortedColumn = sortedColumn.firstIndexOf(ch)

        val byte = BaseCodec.toByte(ch)
        val countBeforeIndices = byteToCountBeforeIndices(byte)
        startI = firstIndexOfChInSortedColumn + countBeforeIndices(startI)
        endI = firstIndexOfChInSortedColumn + countBeforeIndices(endI+1)-1
        if (endI < startI) {
          return 0
        }

      }
      endI-startI+1
    }

  }

  private object BetterBWMatcher {

    def apply(bwtText: String): BetterBWMatcher = {
      val byteToCountBeforeIndices = getByteToCountBeforeIndices(bwtText)
      new BetterBWMatcher(byteToCountBeforeIndices, SortedColumn(bwtText))
    }

    private def getByteToCountBeforeIndices(bwtText: String): Array[Array[Int]] = {
      val byteToCountBeforeIndex = new Array[Array[Int]](5)
      for (i <- byteToCountBeforeIndex.indices) {
        byteToCountBeforeIndex(i) = new Array[Int](bwtText.length + 1)
      }
      for (i <- 1 to bwtText.length) {
        for (j <- byteToCountBeforeIndex.indices) {
          byteToCountBeforeIndex(j)(i) = byteToCountBeforeIndex(j)(i - 1)
        }

        val prevChar = bwtText(i - 1)
        val prevByte = BaseCodec.toByte(prevChar)
        byteToCountBeforeIndex(prevByte)(i) += 1
      }
      byteToCountBeforeIndex
    }

  }
}
