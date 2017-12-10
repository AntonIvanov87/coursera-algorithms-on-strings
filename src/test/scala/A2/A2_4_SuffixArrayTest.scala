package A2

import java.util.concurrent.atomic.LongAdder

import A2.A2_4_SuffixArray.sortedStartIndices
import util.TestBase

class A2_4_SuffixArrayTest extends TestBase {

  test("GAC$") {
    assert(sortedStartIndices("GAC$") === Array(3, 1, 2, 0))
  }

  test("GAGAGAGA$") {
    assert(sortedStartIndices("GAGAGAGA$") === Array(8, 7, 5, 3, 1, 6, 4, 2, 0))
  }

  test("AACGATAGCGGTAGA$") {
    assert(sortedStartIndices("AACGATAGCGGTAGA$") === Array(15, 14, 0, 1, 12, 6, 4, 2, 8, 13, 3, 7, 9, 10, 11, 5))
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(1000)
    val textGen = TestBase.textGen(10000)

    val iteration = new LongAdder
    val totalTimeNs = new LongAdder
    forAll((textGen, "text")) { (gened: String) =>
      val text = gened + '$'

      val start = System.nanoTime

      val suffixStartIndicies = sortedStartIndices(text)

      var prevSuffix = text.substring(suffixStartIndicies(0))
      for(i <- 1 until suffixStartIndicies.length) {
        val curSuffix = text.substring(suffixStartIndicies(i))
        assert(prevSuffix <= curSuffix)
        prevSuffix = curSuffix
      }

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 400) {
        totalTimeNs.add(System.nanoTime - start)
        totalTimeNs.longValue() / (iteration.longValue() - 399) / (1000 * 1000)
      } else {
        0
      }
      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${text.length} symbols in text")
    }
  }
}
