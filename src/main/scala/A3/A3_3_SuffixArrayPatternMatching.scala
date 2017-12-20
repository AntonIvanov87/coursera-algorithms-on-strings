package A3

import A3.A3_2_SuffixArray.sortStartIndices

private object A3_3_SuffixArrayPatternMatching {

  def findStartIndices(text: String, patterns: Set[String]): Set[Int] = {
    val text$ = text + '$'
    val suffixIndices = sortStartIndices(text$)
    patterns.flatMap(
      findStartIndices(_, suffixIndices, text$)
    )
  }

  private def findStartIndices(pattern: String, suffixIndices: Array[Int], text: String): Set[Int] = {
    var minIndex = 0
    var maxIndex = suffixIndices.length-1
    for (i <- pattern.indices) {
      val ch = pattern.charAt(i)
      minIndex = findMinIndex(ch, i, suffixIndices, minIndex, maxIndex, text)
      if (minIndex == -1) {
        return Set()
      }
      maxIndex = findMaxIndex(ch, i, suffixIndices, minIndex, maxIndex, text)
    }
    (minIndex to maxIndex).map(suffixIndices(_)).toSet
  }

  private def findMinIndex(ch: Char, offset: Int, suffixArray: Array[Int], fromI: Int, toI: Int, text: String): Int = {
    var minI = fromI
    var maxI = toI
    while (minI < maxI) {
      val middleI = (minI + maxI) / 2
      val textI = suffixArray(middleI) + offset
      val middleCh = text.charAt(textI)
      if (middleCh > ch) {
        maxI = middleI - 1
      } else if (middleCh == ch) {
        maxI = middleI
      } else {
        minI = middleI + 1
      }
    }
    val textI = suffixArray(minI) + offset
    if (text.charAt(textI) == ch) minI else -1
  }

  private def findMaxIndex(ch: Char, offset: Int, suffixArray: Array[Int], fromI: Int, toI: Int, text: String): Int = {
    var minI = fromI
    var maxI = toI
    while (minI < maxI) {
      val middleI = (minI + maxI + 1) / 2
      val textI = suffixArray(middleI) + offset
      val middleCh = text.charAt(textI)
      if (middleCh > ch) {
        maxI = middleI - 1
      } else if (middleCh == ch) {
        minI = middleI
      } else {
        minI = middleI + 1
      }
    }
    val textI = suffixArray(maxI) + offset
    if (text.charAt(textI) == ch) maxI else -1
  }

}
