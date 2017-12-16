package A1

import A1.A1_5_ShortestSubstring.shortestSubstringOfANotInB
import util.{Stats, TestBase}

class A1_5_ShortestSubstringTest extends TestBase {

  test("A A") {
    assert(shortestSubstringOfANotInB("A", "A") === None)
  }

  test("A T") {
    assert(shortestSubstringOfANotInB("A", "T") === Some("A"))
  }

  test("A AT") {
    assert(shortestSubstringOfANotInB("A", "AT") === None)
  }

  test("AA TT") {
    assert(shortestSubstringOfANotInB("AA", "TT") === Some("A"))
  }

  test("CTCG CGCT") {
    assert(shortestSubstringOfANotInB("CTCG", "CGCT") === Some("TC"))
  }

  test("CCAAGCTGCTAGAGG CATGCTGGGCTGGCT") {
    val shortest = shortestSubstringOfANotInB("CCAAGCTGCTAGAGG", "CATGCTGGGCTGGCT").get
    assert(Set("AA", "AG", "CC", "GA", "TA") contains shortest)
  }

  test("ATGCGATGACCTGACTGA CTCAACGTATTGGCCAGA") {
    val shortest = shortestSubstringOfANotInB("ATGCGATGACCTGACTGA", "CTCAACGTATTGGCCAGA").get
    assert(Set("ACC", "ATG", "CCT", "GAC", "TGA") contains shortest)
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 2000)
    val textGen = TestBase.textGen(2000)

    val stats = new Stats(skipFirstDurations = 200)
    forAll((textGen, "text A"), (textGen, "text B")) { (textA: String, textB: String) =>
      val start = System.nanoTime

      val shortestOption = shortestSubstringOfANotInB(textA, textB)

      stats.addDuration(start)

      if (shortestOption.isEmpty) {
        assert(textB contains textA)
      } else {
        val shortest = shortestOption.get
        assert(textA contains shortest)
        assert(!textB.contains(shortest))
        for(len <- 1 until shortest.length) {
          for(startI <- 0 to textA.length - len) {
            val subA = textA.substring(startI, startI+len)
            assert(textB contains subA)
          }
        }
      }

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., ${textA.length} symbols in A, ${textB.length} in text B")
    }
  }

}
