package A3

import A3.A3_1_KMP.find
import util.{Stats, TestBase}

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
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 100)

    val patternGen = TestBase.textGen(100)
    val textGen = TestBase.textGen(1000000)

    val stats = new Stats(skipFirstDurations = 10)
    forAll((patternGen, "pattern"), (textGen, "text")) { (pattern: String, text: String) =>
      val start = System.nanoTime()

      val patternIndices = find(pattern, text)

      stats.addDuration(start)

      if (patternIndices.isEmpty) {
        assert(!text.contains(pattern))
      } else {
        for (i <- patternIndices) {
          assert(text.indexOf(pattern, i) === i)
        }
      }

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., ${text.length} symbols in text, ${pattern.length} symbols in pattern, found ${patternIndices.length}")
    }
  }

}
