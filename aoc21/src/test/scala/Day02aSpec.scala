import org.scalatest.prop.TableFor3

import scala.util.{Success, Try}

class Day02aSpec extends AoCSpec(Day02a) with Day02CommonSpec {

  override val tests: TableFor3[Part, InputType, Try[Int]] =
    Table[Part, InputType, Try[Int]](testsBase.heading) ++ testsBase
      .map { case (part, inputtype, int) => (part, inputtype, Success(int)) }
}
