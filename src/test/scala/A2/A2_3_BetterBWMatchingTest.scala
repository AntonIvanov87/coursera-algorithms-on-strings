package A2

import A2.A2_3_BetterBWMatching._
import org.scalacheck.Gen
import util.{Stats, TestBase}

class A2_3_BetterBWMatchingTest extends TestBase {

  test("AGGGAA$ GA") {
    assert(bwMatch("AGGGAA$", Array[String]("GA")) === Array(3))
  }

  test("ATT$AA ATA A") {
    assert(bwMatch("ATT$AA", Array[String]("ATA", "A")) === Array(2, 3))
  }

  test("AT$TCTATG TCT TATG") {
    assert(bwMatch("AT$TCTATG", Array[String]("TCT", "TATG")) === Array(0, 0))
  }

  test("check") {

    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 10)

    val textGen = TestBase.textGen(1000000)
    val patternGen = TestBase.textGen(1000)

    val patternsGen: Gen[Array[String]] = for {
      size <- Gen.choose(1, 5000)
      patterns <- Gen.containerOfN[Array, String](size, patternGen)
    } yield patterns

    val stats = new Stats(skipFirstDurations = 3)
    forAll((textGen, "text"), (patternsGen, "patterns")) { (text: String, patterns: Array[String]) =>

      val bwtText = A2_1_BWT.bwt(text + '$')

      val start = System.nanoTime()

      val patternIToCount = bwMatch(bwtText, patterns)

      stats.addDuration(start)

      for (i <- patterns.indices) {
        val pattern = patterns(i)
        assert(patternIToCount(i) === A2_3_BetterBWMatchingTest.countOfSubstring(text, pattern))
      }

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., ${text.length} symbols in text, ${patterns.length} patterns")
    }
  }

}

object A2_3_BetterBWMatchingTest {
  private def countOfSubstring(text: String, pattern: String): Int = {

    var count = 0
    var startI = 0
    do {
      val foundAt = text.indexOf(pattern, startI)
      if (foundAt < 0) {
        return count
      }
      count += 1
      startI = foundAt + 1
    } while (startI < text.length)
    count
  }
}
