// Second approach, which is much simpler.

object Day01b extends Puzzle[Iterable[Int], Int] {
  override def parseAFromLines(lines: Iterable[String]): Iterable[Int] = lines.map(_.toInt)

  override def solvePart1(measurements: Iterable[Int]): Int = {
    val measurementPairs = measurements.sliding(2)
    measurementPairs.count(mp => mp.head < mp.last)
  }

  override def solvePart2(measurements: Iterable[Int]): Int = {
    val measurementTriplesSummed = measurements.sliding(3).map(_.sum)
    val tripleSumPairs = measurementTriplesSummed.sliding(2)
    tripleSumPairs.count(tsp => tsp.head < tsp.last)
  }
}
