package A1

import A1.A1_4_SuffixTrie._
import util.{Stats, TestBase}

class A1_4_SuffixTrieTest extends TestBase {

  test("$") {
    val suffixTrie = new SuffixTrie("$")
    assertTrieHasAllSuffixes(suffixTrie, "$")
    assert(!suffixTrie.has("A"))

    val edges = suffixTrieToEdges(suffixTrie)
    assert(edges === Seq("$"))
  }

  test("A$") {
    val suffixTrie = new SuffixTrie("A$")
    assertTrieHasAllSuffixes(suffixTrie, "A$")
    assert(!suffixTrie.has("C"))

    val edges = suffixTrieToEdges(suffixTrie)
    assert(edges.sorted === Seq("A$", "$").sorted)
  }

  test("ACA$") {
    val suffixTrie = new SuffixTrie("ACA$")
    assertTrieHasAllSuffixes(suffixTrie, "ACA$")
    assert(!suffixTrie.has("CC"))

    val edges = suffixTrieToEdges(suffixTrie)
    assert(edges.sorted === Seq("$", "A", "$", "CA$", "CA$").sorted)
  }

  test("ATAA$") {
    val suffixTrie = new SuffixTrie("ATAA$")
    assertTrieHasAllSuffixes(suffixTrie, "ATAA$")

    val edges = suffixTrieToEdges(suffixTrie)
    assert(edges.sorted === Seq("A", "TAA$", "$", "$", "A$", "TAA$").sorted)
  }

  test("ATAAATG$") {
    val suffixTrie = new SuffixTrie("ATAAATG$")
    assertTrieHasAllSuffixes(suffixTrie, "ATAAATG$")

    val edges = suffixTrieToEdges(suffixTrie)
    assert(edges.sorted === Seq("AAATG$", "G$", "T", "ATG$", "TG$", "A", "A", "AAATG$", "G$", "T", "G$", "$").sorted)
  }

  private def assertTrieHasAllSuffixes(trie: SuffixTrie, text: String): Unit = {
    for(i <- text.indices) {
      assert(trie.has(text.substring(i)))
    }
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 300)

    val textGen = TestBase.textGen(5000)

    val stats = new Stats(skipFirstDurations = 100)
    forAll((textGen, "text")) { (genedText: String) =>
      val text = genedText + '$'

      val start = System.nanoTime

      val suffixTrie = new SuffixTrie(text)
      assertTrieHasAllSuffixes(suffixTrie, text)

      stats.addDuration(start)

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., ${text.length} symbols in text")
    }
  }

}
