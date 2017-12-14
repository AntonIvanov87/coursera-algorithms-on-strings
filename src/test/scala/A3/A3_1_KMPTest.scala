package A3

import java.util.concurrent.atomic.LongAdder

import util.TestBase
import A3_1_KMP.find

class A3_1_KMPTest extends TestBase {

  test("TACG GT") {
    assert(find("TACG", "GT") === Seq())
  }

  test("ATA ATATA") {
    assert(find("ATA", "ATATA") === Seq(0, 2))
  }

  test("ATAT GATATATGCATATACTT") {
    assert(find("ATAT", "GATATATGCATATACTT") === Seq(1, 3, 9))
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(100)

    val patternGen = TestBase.textGen(100)
    val textGen = TestBase.textGen(1000000)

    val iteration = new LongAdder
    val totalTimeMillis = new LongAdder
    forAll((patternGen, "pattern"), (textGen, "text")) { (pattern: String, text: String) =>
      val start = System.currentTimeMillis

      val patternIndices = find(pattern, text)

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 10) {
        totalTimeMillis.add(System.currentTimeMillis - start)
        totalTimeMillis.longValue() / (iteration.longValue() - 9)
      } else {
        0
      }

      if (patternIndices.isEmpty) {
        assert(!text.contains(pattern))
      } else {
        for (i <- patternIndices) {
          assert(text.indexOf(pattern, i) === i)
        }
      }

      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${text.length} symbols in text, ${pattern.length} symbols in pattern, found ${patternIndices.length}")
    }
  }

}
