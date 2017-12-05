package A2

import A2.A2_1_BWT.bwt
import util.TestBase

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

}
