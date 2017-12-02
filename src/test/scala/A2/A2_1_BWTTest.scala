package A2

import java.util.concurrent.atomic.LongAdder

import A2.A2_1_BWT.bwt
import util.TestBase

import scala.collection.mutable

class A2_1_BWTTest extends TestBase {
  import A2_1_BWTTest._

  test("AA$") {
    assert(bwt("AA$") === "AA$")
  }

  test("ACACACAC$") {
    assert(bwt("ACACACAC$") === "CCCC$AAAA")
  }

  test("AGACATA$") {
    assert(bwt("AGACATA$") === "ATG$CAAA")
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(1000)
    val textGen = TestBase.textGen(10000)

    val iteration = new LongAdder
    val totalTimeNs = new LongAdder
    forAll((textGen, "text")) { (gened: String) =>
      val text = gened + '$'

      val start = System.nanoTime

      val bwtResult = bwt(text)

      assert(countChars(bwtResult) === countChars(text))

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 200) {
        totalTimeNs.add(System.nanoTime - start)
        totalTimeNs.longValue() / (iteration.longValue() - 199) / (1000 * 1000)
      } else {
        0
      }
      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${text.size} symbols in text")
    }
  }

}

object A2_1_BWTTest {
  def countChars(text: String): mutable.Map[Char,Int] = {
    text.foldLeft(mutable.Map[Char,Int]().withDefaultValue(0))((m, ch) => {
      m(ch) = m(ch) + 1
      m
    })
  }
}
