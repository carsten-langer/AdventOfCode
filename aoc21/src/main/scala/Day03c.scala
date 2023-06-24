import scala.annotation.{tailrec, unused}

object Day03c extends Puzzle[Iterable[String], Int] {
  override def parseAFromLines(lines: Iterable[String]): Iterable[String] = lines

  // inspired by
  // https://github.com/maneatingape/advent-of-code-scala/blob/main/src/main/scala/AdventOfCode2021/Day03.scala
  override def solvePart1(reports: Iterable[String]): Int = {
    reports
      .transpose
      .map(r => if (r.count(_ == '1') * 2 > r.size) Seq(1, 0) else Seq(0, 1))
      .transpose
      .map(bs => Integer.parseUnsignedInt(bs.mkString, 2))
      .product
  }

  @unused
  def solvePart1Alternative(reports: Iterable[String]): Int = {
    val reportLength = reports.size
    val lineLength = reports.head.length
    val moreOnesThanZeros = Range(0, lineLength).map(i =>
      reports.count(_.charAt(i) == '1') * 2 > reportLength)
    val gammaBits = moreOnesThanZeros.map(if (_) 1 else 0)
    val epsilonBits = moreOnesThanZeros.map(if (_) 0 else 1)
    val gamma = Integer.parseUnsignedInt(gammaBits.mkString, 2)
    val epsilon = Integer.parseUnsignedInt(epsilonBits.mkString, 2)
    gamma * epsilon
  }

  override def solvePart2(reports: Iterable[String]): Int = {

    @tailrec
    def filterToSingleValue(remainingReports: Iterable[String],
                            pos: Int,
                            filterCriterion: (Int, Int, Boolean) => Boolean): Int = {
      if (remainingReports.size == 1)
        Integer.parseUnsignedInt(remainingReports.head, 2)
      else {
        val onesAtPos = remainingReports.count(_.charAt(pos) == '1')
        val threshold = (remainingReports.size + 1) / 2
        val filteredReports = remainingReports.filter(r => filterCriterion(onesAtPos, threshold, r.charAt(pos) == '1'))
        filterToSingleValue(filteredReports, pos + 1, filterCriterion)
      }
    }

    val oxygenGeneratorRating = filterToSingleValue(reports, 0, _ >= _ == _)
    val co2ScrubberRating = filterToSingleValue(reports, 0, _ < _ == _)
    oxygenGeneratorRating * co2ScrubberRating
  }

}
