// Third approach, which is much simpler, and I learned about the power of the unapply method.

import Day02cHelper._

object Day02c extends Puzzle[Iterable[String], Int] {

  override def parseAFromLines(lines: Iterable[String]): Iterable[String] = lines

  override def solvePart1(commands: Iterable[String]): Int = {
    val (final_h, final_d) = commands.foldLeft((0, 0)) {
      case ((horizontal, depth), command) =>
        command match {
          case Forward(amount) => (horizontal + amount, depth)
          case Down(amount) => (horizontal, depth + amount)
          case Up(amount) => (horizontal, depth - amount)
        }
    }
    final_h * final_d
  }

  override def solvePart2(commands: Iterable[String]): Int = {
    val (final_h, final_d, _) = commands.foldLeft((0, 0, 0)) {
      case ((horizontal, depth, aim), command) =>
        command match {
          case Forward(amount) => (horizontal + amount, depth + aim * amount, aim)
          case Down(amount) => (horizontal, depth, aim + amount)
          case Up(amount) => (horizontal, depth, aim - amount)
        }
    }
    final_h * final_d
  }

}


object Day02cHelper {

  // idea from:
  // https://github.com/maneatingape/advent-of-code-scala/blob/main/src/main/scala/AdventOfCode2021/Day02.scala
  sealed case class Command(prefix: String) {
    def unapply(s: String): Option[Int] = Option.when(s.startsWith(prefix))(s.drop(prefix.length + 1).toInt)
  }

  object Forward extends Command("forward")

  object Down extends Command("down")

  object Up extends Command("up")

}
