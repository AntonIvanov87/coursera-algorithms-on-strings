package A1

import scala.collection.mutable

private object A1_2_TrieMatching {

  def getPatternToIndexes(text: String, patterns: Seq[String]): Map[String, Set[Int]] = {
    val trie = Trie(patterns: _*)

    val patternToIndexes = new mutable.HashMap[String, mutable.Set[Int]] with mutable.MultiMap[String, Int]
    for (startIndex <- 0 until text.length) {
      val foundPatterns = getPatternsOfPrefix(text.substring(startIndex), trie)
      for (foundPattern <- foundPatterns) {
        patternToIndexes.addBinding(foundPattern, startIndex)
      }
    }
    patternToIndexes.mapValues(_.toSet).toMap
  }

  private def getPatternsOfPrefix(text: String, trie: Trie): Set[String] = {
    val foundPatterns = mutable.Set[String]()
    var curNode: Trie.Node = trie.root
    for (curIndexInText <- 0 until text.length) {
      val curBase = text.charAt(curIndexInText)
      val nextNodeOption = curNode.nextNode(curBase)
      if (nextNodeOption.isEmpty) {
        return foundPatterns.toSet
      }
      val nextNode = nextNodeOption.get
      if (nextNode.isEndOfPattern) {
        foundPatterns.add(text.substring(0, curIndexInText + 1))
      }
      curNode = nextNode
    }
    foundPatterns.toSet
  }

}
