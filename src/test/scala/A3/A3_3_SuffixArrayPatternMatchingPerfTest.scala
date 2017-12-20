package A3

import A3.A3_3_SuffixArrayPatternMatching.findStartIndices
import org.scalacheck.Gen
import util.{Stats, TestBase}

private object A3_3_SuffixArrayPatternMatchingPerfTest extends TestBase {

  def main(args: Array[String]) {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 100)

    val textGen = TestBase.newFixedSizeTextGen(100000)
    val patternGen = TestBase.textGen(1000)

    val patternsGen: Gen[Set[String]] = Gen.containerOfN[Set, String](10000, patternGen)

    val stats = new Stats(skipFirstDurations = 3)
    forAll((textGen, "text"), (patternsGen, "patterns")) { (text: String, patterns: Set[String]) =>

      val start = System.nanoTime()

      val startIndices = findStartIndices(text, patterns)

      stats.addDuration(start)

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., black hole ${startIndices.sum}")
    }

  }

}
