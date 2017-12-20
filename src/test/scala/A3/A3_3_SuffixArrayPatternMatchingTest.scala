package A3

import A3.A3_3_SuffixArrayPatternMatching.findStartIndices
import org.scalacheck.Gen
import util.TestBase

class A3_3_SuffixArrayPatternMatchingTest extends TestBase {

  test("A in AAA") {
    assert(findStartIndices("AAA", Set("A")) === Set(0, 1, 2))
  }

  test("C,G,C in ATA") {
    assert(findStartIndices("ATA", Set("C", "G", "C")) === Set())
  }

  test("ATA,C,TATAT in ATATATA") {
    assert(findStartIndices("ATATATA", Set("ATA", "C", "TATAT")) === Set(4, 2, 0, 1))
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 100)

    val textGen = TestBase.textGen(100000)
    val patternGen = TestBase.textGen(1000)

    val patternsGen: Gen[Set[String]] = for {
      size <- Gen.choose(1, 100)
      patterns <- Gen.containerOfN[Set, String](size, patternGen)
    } yield patterns

    forAll((textGen, "text"), (patternsGen, "patterns")) { (text: String, patterns: Set[String]) =>

      val startIndices = findStartIndices(text, patterns)

      val expectedStartIndices = patterns.flatMap(pattern => {
        var fromIndex = 0
        var patternStartIndices = Set[Int]()
        while (fromIndex <= text.length - pattern.length) {
          val startIndex = text.indexOf(pattern, fromIndex)
          if (startIndex == -1) {
            fromIndex = text.length
          } else {
            patternStartIndices += startIndex
            fromIndex = startIndex + 1
          }
        }
        patternStartIndices
      })

      assert(startIndices === expectedStartIndices)
    }
  }

}
