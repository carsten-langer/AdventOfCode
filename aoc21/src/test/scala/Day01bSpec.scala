import org.scalatest.prop.TableFor3

class Day01bSpec extends AoCSpec(Day01b) with Day01CommonSpec {
  override val tests: TableFor3[Part, InputType, Int] = testsBase
}
