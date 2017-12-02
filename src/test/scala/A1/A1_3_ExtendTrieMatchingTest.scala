package A1

import java.util.concurrent.atomic.LongAdder

import A1.A1_2_TrieMatching._
import org.scalacheck.Gen
import util.TestBase

class A1_3_ExtendTrieMatchingTest extends TestBase {

  test("text AA, pattern T") {
    val patternToIndexes = getPatternToIndexes("AA", Seq("T"))
    assert(patternToIndexes === Map())
  }

  test("text ACATA, patterns AT A AG") {
    val patternToIndexes = getPatternToIndexes("ACATA", Seq("AT", "A", "AG"))
    assert(patternToIndexes === Map("AT" -> Set(2), "A" -> Set(0, 2, 4)))
  }

  test("text GCACTT, patterns C, CT") {
    val patternToIndexes = getPatternToIndexes("GCACTT", Seq("C", "CT"))
    assert(patternToIndexes === Map("C" -> Set(1, 3), "CT" -> Set(3)))
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(1000)

    val textGen = TestBase.textGen(10000)
    val patternGen = TestBase.textGen(100)

    val patternsGen: Gen[List[String]] = for {
      size <- Gen.choose(1, 5000)
      patterns <- Gen.listOfN(size, patternGen)
    } yield patterns

    val iteration = new LongAdder
    val totalTimeMillis = new LongAdder
    forAll((textGen, "text"), (patternsGen, "patterns")) { (text: String, patterns: List[String]) =>
      val start = System.currentTimeMillis

      val patternsToIndexes = getPatternToIndexes(text, patterns)

      for ((pattern, indexes) <- patternsToIndexes) {
        for (index <- indexes) {
          assert(text.substring(index, index + pattern.length) === pattern)
        }
      }

      val patternsNotInText = patterns.filterNot(pattern => patternsToIndexes.contains(pattern) || pattern.isEmpty)
      for (patternNotInText <- patternsNotInText) {
        assert(!text.contains(patternNotInText))
      }

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 200) {
        totalTimeMillis.add(System.currentTimeMillis - start)
        totalTimeMillis.longValue() / (iteration.longValue() - 199)
      } else {
        0
      }
      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${text.size} symbols in text, ${patterns.size} patterns")
    }
  }
}
