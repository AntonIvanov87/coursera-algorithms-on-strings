import java.util.concurrent.atomic.LongAdder

import A1_2_TrieMatching._
import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class A1_3_ExtendTrieMatchingTest extends FunSuite with GeneratorDrivenPropertyChecks {

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
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(
      minSuccessful = 1000,
      workers = PosInt.from(Runtime.getRuntime.availableProcessors).get
    )

    val textGen = Tests.textGen(10000)
    val patternGen = Tests.textGen(100)

    val patternsGen: Gen[List[String]] = for {
      size <- Gen.choose(1, 5000)
      patterns <- Gen.listOfN(size, patternGen)
    } yield patterns

    val iteration = new LongAdder
    val totalTimeMillis = new LongAdder
    forAll((textGen, "text"), (patternsGen, "patterns")) { (genedText: String, genedPatterns: List[String]) =>
      val text = Tests.filterGened(genedText)
      val patterns = genedPatterns.map(Tests.filterGened)

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
