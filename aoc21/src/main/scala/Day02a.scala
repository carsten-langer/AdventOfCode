// Taken from original AoC 2021 solutions, including most of the error handling parts.

import Day02aHelper._
import cats.implicits.toTraverseOps

import scala.util.{Failure, Try}

object Day02a extends Puzzle[Try[Iterable[Command]], Try[Int]] {

  override def parseAFromLines(lines: Iterable[String]): Try[Iterable[Command]] = {
    lines.toSeq.traverse { line =>
      val Seq(action, amount) = line.split(' ').toSeq
      Command(action, amount)
    }
  }

  override def solvePart1(commandsT: Try[Iterable[Command]]): Try[Int] = commandsT.map { commands =>
    val finalPosition = commands.foldLeft(Position.position0)((position, command) => position.move1(command))
    finalPosition.challengeProduct
  }

  override def solvePart2(commandsT: Try[Iterable[Command]]): Try[Int] = commandsT.map { commands =>
    val finalPosition = commands.foldLeft(Position.position0)((position, command) => position.move2(command))
    finalPosition.challengeProduct
  }

}


object Day02aHelper {

  sealed trait Action

  private case object Forward extends Action

  private case object Up extends Action

  private case object Down extends Action


  final case class Command private(action: Action, amount: Int) extends Uncopyable

  object Command {
    def apply(action: String, amount: String): Try[Command] =
      action match {
        case "forward" => Command(Forward, amount)
        case "down" => Command(Down, amount)
        case "up" => Command(Up, amount)
        case _ => Failure(new IllegalArgumentException("Unknown command string!"))
      }

    def apply(action: Action, amount: String): Try[Command] = Try(amount.toInt).flatMap(Command(action, _))

    def apply(action: Action, amount: Int): Try[Command] = Try(require(amount >= 0)).map(_ => new Command(action, amount))
  }


  final case class Position private(horizontal: Int, depth: Int, aim: Int) extends Uncopyable {
    // Submarine cannot rise above sea level, thus cap new depth to be minimum 0.
    def move1(command: Command): Position = command match {
      case Command(Forward, amount) => new Position(horizontal + amount, depth, aim)
      case Command(Down, amount) => new Position(horizontal, depth + amount, aim)
      case Command(Up, amount) => new Position(horizontal, math.max(0, depth - amount), aim)
    }

    def move2(command: Command): Position = command match {
      case Command(Forward, amount) => new Position(horizontal + amount, math.max(0, depth + aim * amount), aim)
      case Command(Down, amount) => new Position(horizontal, depth, aim + amount)
      case Command(Up, amount) => new Position(horizontal, depth, aim - amount)
    }

    def challengeProduct: Int = horizontal * depth
  }

  object Position {
    def apply(horizontal: Int, depth: Int, aim: Int): Try[Position] =
      Try(require(depth >= 0)).map(_ => new Position(horizontal, depth, aim))

    def position0: Position = new Position(0, 0, 0)
  }

}
