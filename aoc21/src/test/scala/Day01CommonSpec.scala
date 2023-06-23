import org.scalatest.prop.TableFor3
import org.scalatest.prop.Tables.Table

trait Day01CommonSpec {

  val testsBase: TableFor3[Part, InputType, Int] = Table[Part, InputType, Int](
    ("puzzle part", "input type", "expected result"),
    (Part1, TestInput, 7),
    (Part1, RealInput, 1475),
    (Part2, TestInput, 5),
    (Part2, RealInput, 1516),
  )

}
