import org.scalatest.prop.TableFor3
import org.scalatest.prop.Tables.Table

trait Day03CommonSpec {

  val testsBase: TableFor3[Part, InputType, Int] = Table[Part, InputType, Int](
    ("puzzle part", "input type", "expected result"),
    (Part1, TestInput, 198),
    (Part1, RealInput, 3687446),
    (Part2, TestInput, 230),
    (Part2, RealInput, 4406844),
  )

}
