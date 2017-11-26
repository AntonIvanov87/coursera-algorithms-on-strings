import java.util.concurrent.atomic.LongAdder

import A1_5_ShortestSubstring.shortestSubstringOfANotInB
import org.scalactic.anyvals.PosInt
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class A1_5_ShortestSubstringTest extends FunSuite with GeneratorDrivenPropertyChecks {

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
    assert(Set("AA", "AG", "CC", "GA") contains shortest)
  }

  test("ATGCGATGACCTGACTGA CTCAACGTATTGGCCAGA") {
    val shortest = shortestSubstringOfANotInB("ATGCGATGACCTGACTGA", "CTCAACGTATTGGCCAGA").get
    assert(Set("ACC", "ATG", "CCT", "GAC", "TGA") contains shortest)
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(
      minSuccessful = 10000,
      workers = PosInt.from(Runtime.getRuntime.availableProcessors).get
    )

    val textGen = Tests.textGen(2000)

    val iteration = new LongAdder
    val totalTimeMillis = new LongAdder
    forAll((textGen, "text A"), (textGen, "text B")) { (genedA: String, genedB: String) =>
      val textA = Tests.filterGened(genedA)
      val textB = Tests.filterGened(genedB)

      val start = System.currentTimeMillis

      val shortestOption = shortestSubstringOfANotInB(textA, textB)
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

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 200) {
        totalTimeMillis.add(System.currentTimeMillis - start)
        totalTimeMillis.longValue() / (iteration.longValue() - 199)
      } else {
        0
      }
      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${textA.size} symbols in A, ${textB.size} in text B")
    }
  }

}
