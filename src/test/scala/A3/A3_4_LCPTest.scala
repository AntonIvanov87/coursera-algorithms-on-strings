package A3

import A3.A3_4_SuffixTree.calcLCPs
import util.TestBase

class A3_4_LCPTest extends TestBase {

  test("$") {
    assert(A3_4_SuffixTree.calcLCPs(Array(0), "$") === Array.emptyIntArray)
  }

  test("A$") {
    assert(calcLCPs(Array(1, 0), "A$") === Array(0))
  }

  test("AA$") {
    assert(calcLCPs(Array(2, 1, 0), "AA$") === Array(0, 1))
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 100)

    val textGen = TestBase.textGen(100000)

    forAll((textGen, "text")) { (genedText: String) =>
      val text = genedText + '$'
      val sortedSuffixes = A3_2_SuffixArray.sortStartIndices(text)

      val lcps = calcLCPs(sortedSuffixes, text)

      for (i <- 0 until sortedSuffixes.length-1) {
        var lcp = 0
        while(text.charAt(sortedSuffixes(i)+lcp) == text.charAt(sortedSuffixes(i+1)+lcp)) {
          lcp+=1
        }
        assert(lcps(i) === lcp)
      }
    }
  }

}
