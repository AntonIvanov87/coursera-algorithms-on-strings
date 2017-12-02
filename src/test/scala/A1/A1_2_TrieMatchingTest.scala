package A1

import A1.A1_2_TrieMatching._
import org.scalatest.FunSuite

class A1_2_TrieMatchingTest extends FunSuite {

  test("AAA AA") {
    val patternToIndexes = getPatternToIndexes("AAA", Seq("AA"))
    assert(patternToIndexes === Map("AA" -> Set(0, 1)))
  }

  test("AA T") {
    val patternToIndexes = getPatternToIndexes("AA", Seq("T"))
    assert(patternToIndexes === Map())
  }

  test("AATCGGGTTCAATCGGGGT ATCG GGGT") {
    val patternToIndexes = getPatternToIndexes("AATCGGGTTCAATCGGGGT", Seq("ATCG", "GGGT"))
    assert(patternToIndexes === Map("ATCG" -> Set(1, 11), "GGGT" -> Set(4, 15)))
  }

}
