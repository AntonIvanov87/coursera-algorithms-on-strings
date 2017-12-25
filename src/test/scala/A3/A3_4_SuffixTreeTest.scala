package A3

import A3.A3_4_SuffixTree.SuffixTree
import util.TestBase

class A3_4_SuffixTreeTest extends TestBase {

  test("$") {
    val text = "$"
    val st = SuffixTree(text)
    assertTrieHasAllSuffixes(st, text)
    assert(!st.has("A"))
    assert(st.getEdges === Seq((0,1)))
  }

  test("A$") {
    val text = "A$"
    val st = SuffixTree(text)
    assertTrieHasAllSuffixes(st, text)
    assert(!st.has("C$"))
    assert(!st.has("A"))
    assert(st.getEdges === Seq((1,2),(0,2)))
  }

  test("AAA$") {
    val text = "AAA$"
    val st = SuffixTree(text)
    assertTrieHasAllSuffixes(st, text)
    assert(!st.has("A"))
    assert(!st.has("AA"))
    assert(!st.has("AAA"))

    val toSubstr = (p: (Int, Int)) => text.substring(p._1,p._2)
    assert(st.getEdges.map(toSubstr) === Seq((3,4),(0,1),(3,4),(1,2),(3,4),(2,4)).map(toSubstr))
  }

  test("GTAGT$") {
    val text = "GTAGT$"
    val st = SuffixTree(text)
    assertTrieHasAllSuffixes(st, text)

    val toSubstr = (p: (Int, Int)) => text.substring(p._1,p._2)
    assert(st.getEdges.map(toSubstr) === Seq((5,6),(2,6),(3,5),(5,6),(2,6),(4,5),(5,6),(2,6)).map(toSubstr))
  }

  private def assertTrieHasAllSuffixes(trie: SuffixTree, text: String): Unit = {
    for(i <- text.indices) {
      val suffixText = text.substring(i)
      assert(trie.has(suffixText), s"trie does not has $suffixText")
    }
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 30)

    val textGen = TestBase.textGen(20000)

    forAll((textGen, "text")) { (genedText: String) =>
      val text = genedText + '$'

      val st = SuffixTree(text)
      assertTrieHasAllSuffixes(st, text)
    }
  }

}
