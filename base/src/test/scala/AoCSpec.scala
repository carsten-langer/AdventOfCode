import org.scalatest.Assertion
import org.scalatest.matchers.should
import org.scalatest.prop.TableFor3
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.io.Source

abstract case class AoCSpec[A, B](puzzle: Puzzle[A, B])
  extends AnyWordSpec
    with should.Matchers
    with ScalaCheckPropertyChecks {

  def tests: TableFor3[Part, InputType, B]

  puzzle.getClass.getSimpleName.init should {
    "solve given parts for given inputs" in {
      forAll(tests)(doTest)
    }
  }

  def doTest(part: Part, inputType: InputType, expectedResult: B): Assertion = {
    val input = inputLines(inputType)
    part match {
      case Part1 => puzzle.solvePart1FromLines(input) shouldEqual expectedResult
      case Part2 => puzzle.solvePart2FromLines(input) shouldEqual expectedResult
    }
  }

  private def inputLines(inputType: InputType): Iterable[String] = {
    val puzzleClass = puzzle.getClass
    val day = puzzleClass.getSimpleName.slice(3, 5)
    val resourceName = s"day${day}_${inputType.suffix}.txt"
    val source = Source.fromResource(resourceName)
    source.getLines().iterator.to(Iterable)
  }
}


sealed trait Part

case object Part1 extends Part

case object Part2 extends Part


sealed trait InputType {
  def suffix: String
}

case object RealInput extends InputType {
  override def suffix: String = "real"
}

case object TestInput extends InputType {
  override def suffix: String = "test"
}
