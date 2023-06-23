// Taken from original AoC 2021 solutions, including most of the error handling parts.

import cats.data.NonEmptyList

import scala.util.Try

object Day01a extends Puzzle[Try[NonEmptyList[Int]], Try[Int]] {
  override def parseAFromLines(lines: Iterable[String]): Try[NonEmptyList[Int]] =
    Try(NonEmptyList.fromListUnsafe(lines.toList).map(_.toInt))

  override def solvePart1(measurementsT: Try[NonEmptyList[Int]]): Try[Int] = measurementsT.map(countIncrements)

  override def solvePart2(measurementsT: Try[NonEmptyList[Int]]): Try[Int] = measurementsT.map(countIncrementsInGroups(3))

  private def countIncrements(measurements: NonEmptyList[Int]): Int = {
    val NonEmptyList(head, tail) = measurements
    val (totalIncrements, _) = tail.foldLeft((0, head)) {
      case ((accIncrements, previousValue), currentValue) if previousValue < currentValue =>
        (accIncrements + 1, currentValue)
      case ((accIncrements, _), currentValue) => (accIncrements, currentValue)
    }
    totalIncrements
  }

  private def countIncrementsInGroups(groupSize: Int)(measurements: NonEmptyList[Int]): Int =
    countIncrements(slidingSum(measurements, groupSize))
  private def slidingSum[A: Numeric](nel: NonEmptyList[A], groupSize: Int): NonEmptyList[A] = {
    val slidingIterator = nel.toList.sliding(groupSize)
    assume(slidingIterator.nonEmpty) // because nel is non-empty
    val slidingNel = NonEmptyList.fromListUnsafe(slidingIterator.toList) // safe because slidingIterator is non-empty
    slidingNel.map(_.sum)
  }

}
