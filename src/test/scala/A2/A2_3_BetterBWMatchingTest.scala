package A2

import java.util.concurrent.atomic.LongAdder

import util.TestBase
import A2_3_BetterBWMatching._
import org.scalacheck.Gen

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

    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(10)

    val textGen = TestBase.textGen(1000000)
    val patternGen = TestBase.textGen(1000)

    val patternsGen: Gen[Array[String]] = for {
      size <- Gen.choose(1, 5000)
      patterns <- Gen.containerOfN[Array, String](size, patternGen)
    } yield patterns

    val iteration = new LongAdder
    val totalTimeMillis = new LongAdder
    forAll((textGen, "text"), (patternsGen, "patterns")) { (text: String, patterns: Array[String]) =>
      val start = System.currentTimeMillis

      val bwtText = A2_1_BWT.bwt(text + '$')
      val patternIToCount = bwMatch(bwtText, patterns)

      for (i <- patterns.indices) {
        val pattern = patterns(i)
        assert(patternIToCount(i) === A2_3_BetterBWMatchingTest.countOfSubstring(text, pattern))
      }

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 3) {
        totalTimeMillis.add(System.currentTimeMillis - start)
        totalTimeMillis.longValue() / (iteration.longValue() - 2)
      } else {
        0
      }
      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${text.size} symbols in text, ${patterns.size} patterns")
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
