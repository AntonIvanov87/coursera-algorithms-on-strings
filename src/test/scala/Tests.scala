import org.scalacheck.Gen
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object Tests {

  val bases = Set('A', 'C', 'G', 'T')

  def textGen(max: Int): Gen[String] = {
    val baseGen = Gen.oneOf(bases.toSeq)
    for {
      size <- Gen.choose(1, max)
      text <- Gen.listOfN(size, baseGen)
    } yield text.mkString
  }

  // after failure of an iteration scalacheck's generators are broken
  def filterGened(text: String): String = {
    text.filter(bases.contains)
  }

}
