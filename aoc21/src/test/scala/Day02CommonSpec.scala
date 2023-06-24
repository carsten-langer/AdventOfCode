import org.scalatest.prop.TableFor3
import org.scalatest.prop.Tables.Table

trait Day02CommonSpec {

  val testsBase: TableFor3[Part, InputType, Int] = Table[Part, InputType, Int](
    ("puzzle part", "input type", "expected result"),
    (Part1, TestInput, 150),
    (Part1, RealInput, 1635930),
    (Part2, TestInput, 900),
    (Part2, RealInput, 1781819478),
  )

}
