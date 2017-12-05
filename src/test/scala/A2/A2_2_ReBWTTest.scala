package A2

import java.util.concurrent.atomic.LongAdder

import A2.A2_1_BWT.bwt
import A2.A2_2_ReBWT._
import util.TestBase

class A2_2_ReBWTTest extends TestBase {

  test("AC$A") {
    assert(reBWT("AC$A") === "ACA$")
  }

  test("AGGGAA$") {
    assert(reBWT("AGGGAA$") === "GAGAGA$")
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(30)
    val textGen = TestBase.textGen(1000000)

    val iteration = new LongAdder
    val totalTimeNs = new LongAdder
    forAll((textGen, "text")) { (gened: String) =>
      val text = gened + '$'

      val start = System.nanoTime

      val bwtResult = bwt(text)

      val reBWTText = reBWT(bwtResult)

      assert(reBWTText === text)

      iteration.increment()
      val msPerIter = if (iteration.intValue() >= 10) {
        totalTimeNs.add(System.nanoTime - start)
        totalTimeNs.longValue() / (iteration.longValue() - 9) / (1000 * 1000)
      } else {
        0
      }
      println(s"Done $iteration of ${generatorDrivenConfig.minSuccessful.value}, $msPerIter ms/iter avg., ${text.size} symbols in text")
    }
  }

}
