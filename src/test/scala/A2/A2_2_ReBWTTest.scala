package A2

import A2.A2_1_BWT.bwt
import A2.A2_2_ReBWT._
import util.{Stats, TestBase}

class A2_2_ReBWTTest extends TestBase {

  test("AC$A") {
    assert(reBWT("AC$A") === "ACA$")
  }

  test("AGGGAA$") {
    assert(reBWT("AGGGAA$") === "GAGAGA$")
  }

  test("check") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 30)
    val textGen = TestBase.textGen(1000000)

    val stats = new Stats(skipFirstDurations = 10)
    forAll((textGen, "text")) { (gened: String) =>
      val text = gened + '$'
      val bwtResult = bwt(text)

      val start = System.nanoTime()

      val reBWTText = reBWT(bwtResult)

      stats.addDuration(start)

      assert(reBWTText === text)

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., ${text.length} symbols in text")
    }
  }

}
