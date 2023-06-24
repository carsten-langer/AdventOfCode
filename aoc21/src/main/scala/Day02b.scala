// Taken from original AoC 2021 solutions, but simplified so that no error handling is involved anymore.
// Still some elaborated transformation from String to Domain is executed.
// Also, instead having 1 type of Position with 2 different move methods, I now have 2 types of Positions,
// which seems more appropriate, as the first Position type does not know about the "aim".

import Day02bHelper._

import scala.annotation.unused

object Day02b extends Puzzle[Iterable[Command], Int] {

  override def parseAFromLines(lines: Iterable[String]): Iterable[Command] = {
    lines.map { line =>
      val Seq(action, amount) = line.split(' ').toSeq
      Command(action, amount)
    }
  }

  override def solvePart1(commands: Iterable[Command]): Int = solvePart(Position1.pos0)(commands)

  override def solvePart2(commands: Iterable[Command]): Int = solvePart(Position2.pos0)(commands)

  private def solvePart(pos0: Position)(commands: Iterable[Command]): Int = {
    val finalPosition = commands.foldLeft(pos0)((pos, cmd) => pos.move(cmd))
    finalPosition.puzzleSolution
  }

}


object Day02bHelper {

  sealed trait Action

  private case object Forward extends Action

  private case object Up extends Action

  private case object Down extends Action


  final case class Command(action: Action, amount: Int) extends Uncopyable

  object Command {
    def apply(action: String, amount: String): Command = {
      val amountInt = amount.toInt
      action match {
        case "forward" => Command(Forward, amountInt)
        case "down" => Command(Down, amountInt)
        case "up" => Command(Up, amountInt)
      }
    }
  }


  trait Position {
    def horizontal: Int

    def depth: Int

    def move(command: Command): Position

    def puzzleSolution: Int = horizontal * depth
  }

  //noinspection ScalaWeakerAccess Avoid false IDE warning
  @unused // Avoid false IDE warning
  final case class Position1(horizontal: Int, depth: Int) extends Position with Uncopyable {
    override def move(command: Command): Position = command match {
      case Command(Forward, amount) => Position1(horizontal + amount, depth)
      case Command(Down, amount) => Position1(horizontal, depth + amount)
      case Command(Up, amount) => Position1(horizontal, depth - amount)
    }
  }

  object Position1 {
    def pos0: Position = Position1(0, 0)
  }

  //noinspection ScalaWeakerAccess Avoid false IDE warning
  @unused // Avoid false IDE warning
  final case class Position2(horizontal: Int, depth: Int, aim: Int) extends Position with Uncopyable {
    override def move(command: Command): Position = command match {
      case Command(Forward, amount) => Position2(horizontal + amount, depth + aim * amount, aim)
      case Command(Down, amount) => Position2(horizontal, depth, aim + amount)
      case Command(Up, amount) => Position2(horizontal, depth, aim - amount)
    }
  }

  object Position2 {
    def pos0: Position = Position2(0, 0, 0)
  }

}
