import org.scalatest.prop.TableFor3

import scala.util.{Success, Try}

class Day03aSpec extends AoCSpec(Day03a) with Day03CommonSpec {

  override val tests: TableFor3[Part, InputType, Try[Int]] =
    Table[Part, InputType, Try[Int]](testsBase.heading) ++ testsBase
      .map { case (part, inputtype, int) => (part, inputtype, Success(int)) }
}
