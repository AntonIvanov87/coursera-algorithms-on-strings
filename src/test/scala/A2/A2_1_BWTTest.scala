package A2

import A2.A2_1_BWT.bwt
import A2.A2_2_ReBWT.reBWT
import util.{Stats, TestBase}

class A2_1_BWTTest extends TestBase {

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
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = propCheckConfig(minSuccesses = 30)
    val textGen = TestBase.textGen(1000000)

    val stats = new Stats(skipFirstDurations = 10)
    forAll((textGen, "text")) { (gened: String) =>
      val text = gened + '$'

      val start = System.nanoTime()

      val bwtResult = bwt(text)

      stats.addDuration(start)

      val reBWTText = reBWT(bwtResult)
      assert(reBWTText === text)

      println(s"Done ${stats.count} of ${generatorDrivenConfig.minSuccessful.value}, ${stats.avgMs} ms/iter avg., ${text.length} symbols in text")
    }
  }


}
