package util

import org.scalacheck.{Gen, Shrink}
import org.scalactic.anyvals.PosInt
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait TestBase extends FunSuite with GeneratorDrivenPropertyChecks {

  implicit val noShrink: Shrink[String] = Shrink.shrinkAny

  protected def propCheckConfig(minSuccesses: Int): PropertyCheckConfiguration = {
    PropertyCheckConfiguration(
      minSuccessful = PosInt.from(minSuccesses).get,
      workers = PosInt.from(Runtime.getRuntime.availableProcessors).get
    )
  }

}

object TestBase {

  private val bases = Set('A', 'C', 'G', 'T')

  def textGen(max: Int): Gen[String] = {
    val baseGen = newBaseGen()
    for {
      size <- Gen.choose(1, max)
      text <- Gen.listOfN(size, baseGen)
    } yield text.mkString
  }

  def newFixedSizeTextGen(size: Int): Gen[String] = {
    val baseGen = Gen.oneOf(bases.toSeq)
    for {
      text <- Gen.listOfN(size, baseGen)
    } yield text.mkString
  }

  private def newBaseGen(): Gen[Char] = {
    Gen.oneOf(bases.toSeq)
  }

}