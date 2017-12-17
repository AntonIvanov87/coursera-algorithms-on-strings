package A3

import A2.A2_4_SuffixArray
import A3.A3_2_SuffixArray.sortStartIndices
import util.{Stats, TestBase}

class A3_2_SuffixArrayTest extends TestBase {

  test("AAA$") {
    assert(sortStartIndices("AAA$") === Array(3, 2, 1, 0))
  }

  test("GAC$") {
    assert(sortStartIndices("GAC$") === Array(3, 1, 2, 0))
  }

  test("GAGAGAGA$") {
    assert(sortStartIndices("GAGAGAGA$") === Array(8, 7, 5, 3, 1, 6, 4, 2, 0))
  }

  test("AACGATAGCGGTAGA$") {
    assert(sortStartIndices("AACGATAGCGGTAGA$") === Array(15, 14, 0, 1, 12, 6, 4, 2, 8, 13, 3, 7, 9, 10, 11, 5))
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 30)
    val textGen = TestBase.textGen(2 * 100000)

    val stats = new Stats(skipFirstDurations = 40)
    forAll((textGen, "text")) { (gened: String) =>
      val text = gened + '$'

      val start = System.nanoTime

      val suffixStartIndicies = sortStartIndices(text)
//      val suffixStartIndicies = A2_4_SuffixArray.sortedStartIndices(text)

      stats.addDuration(start)

      var prevSuffix = text.substring(suffixStartIndicies(0))
      for(i <- 1 until suffixStartIndicies.length) {
        val curSuffix = text.substring(suffixStartIndicies(i))
        assert(prevSuffix <= curSuffix)
        prevSuffix = curSuffix
      }

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., ${text.length} symbols in text")
    }
  }

}
