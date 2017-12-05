package A1

private object A1_5_ShortestSubstring {

  def shortestSubstringOfANotInB(textA: String, textB: String): Option[String] = {
    val trieA = new SuffixTrie(textA + '$')
    val trieB = new SuffixTrie(textB + '$')
    for (substringA <- trieA.substrings) {
      if (!trieB.has(substringA) && !substringA.endsWith("$")) {
        return Some(substringA)
      }
    }
    None
  }

}
