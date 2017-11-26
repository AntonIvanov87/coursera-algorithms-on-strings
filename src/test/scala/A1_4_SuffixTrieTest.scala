import java.util.concurrent.atomic.LongAdder

import A1_4_SuffixTrie._
import org.scalactic.anyvals.PosInt
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class A1_4_SuffixTrieTest extends FunSuite with GeneratorDrivenPropertyChecks {

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
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(
      minSuccessful = 1000,
      workers = PosInt.from(Runtime.getRuntime.availableProcessors).get
    )

    val textGen = Tests.textGen(5000)

    val iteration = new LongAdder
    val totalTimeMillis = new LongAdder
    forAll((textGen, "text")) { (genedText: String) =>
      val text = Tests.filterGened(genedText) + '$'

      val start = System.currentTimeMillis

      val suffixTrie = new SuffixTrie(text)
      assertTrieHasAllSuffixes(suffixTrie, text)

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 200) {
        totalTimeMillis.add(System.currentTimeMillis - start)
        totalTimeMillis.longValue() / (iteration.longValue() - 199)
      } else {
        0
      }
      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${text.size} symbols in text")
    }
  }

}
