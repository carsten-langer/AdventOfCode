// Taken from original AoC 2021 solutions, but simplified so that no error handling is involved anymore.
// Still some elaborated transformation from String to Domain is executed.

import Day03bHelper._

import scala.annotation.tailrec

object Day03b extends Puzzle[DiagnosticReport, Int] {

  override def parseAFromLines(lines: Iterable[String]): DiagnosticReport =
    DiagnosticReport(lines.map(DiagnosticBits(_)).toSeq)

  override def solvePart1(diagnosticReport: DiagnosticReport): Int = {
    val positions = diagnosticReport.positions
    val gammaBits = positions.map(pos => diagnosticReport.getMostCommonBitAtPosition(pos).get)
    val epsilonBits = gammaBits.map(_.flipBit)
    val gamma = valueOf(gammaBits)
    val epsilon = valueOf(epsilonBits)
    gamma * epsilon
  }

  override def solvePart2(diagnosticReport: DiagnosticReport): Int = {
    val oxygenGeneratorRatingBits = filterForMostCommonBitWithDefaultOne(diagnosticReport)
    val co2ScrubberRatingBits = filterForLeastCommonBitWithDefaultZero(diagnosticReport)
    val oxygenGeneratorRating = valueOf(oxygenGeneratorRatingBits)
    val co2ScrubberRating = valueOf(co2ScrubberRatingBits)
    oxygenGeneratorRating * co2ScrubberRating
  }

  private def filterForMostCommonBitWithDefaultOne(diagnosticReport: DiagnosticReport): DiagnosticBits =
    filterToSingleValue(diagnosticReport, 0, getMostCommonBitAtPositionWithDefaultOne)

  private def filterForLeastCommonBitWithDefaultZero(diagnosticReport: DiagnosticReport): DiagnosticBits =
    filterToSingleValue(diagnosticReport, 0, getLeastCommonBitAtPositionWithDefaultZero)

  @tailrec
  private def filterToSingleValue(diagnosticReport: DiagnosticReport,
                                  pos: Int,
                                  getRelevantBit: (DiagnosticReport, Int) => Bit): DiagnosticBits =
    diagnosticReport.diagnosticBitss match {
      case Seq(head) => head
      case diagnosticBitss =>
        val relevantBitAtPos = getRelevantBit(diagnosticReport, pos)
        val filteredDiagnosticBitss = diagnosticBitss.filter(_(pos) == relevantBitAtPos)
        val filteredDiagnosticReport = DiagnosticReport(filteredDiagnosticBitss)
        filterToSingleValue(filteredDiagnosticReport, pos + 1, getRelevantBit)
    }

  private def getMostCommonBitAtPosition(diagnosticReport: DiagnosticReport, pos: Int, tiesResult: Bit): Bit =
    diagnosticReport.getMostCommonBitAtPosition(pos).getOrElse(tiesResult)

  private def getMostCommonBitAtPositionWithDefaultOne(diagnosticReport: DiagnosticReport, pos: Int): Bit =
    getMostCommonBitAtPosition(diagnosticReport, pos, One)

  private def getLeastCommonBitAtPositionWithDefaultZero(diagnosticReport: DiagnosticReport, pos: Int): Bit =
    getMostCommonBitAtPositionWithDefaultOne(diagnosticReport, pos).flipBit

  private def valueOf(bits: Iterable[Bit]): Int = {
    val bitChars = bits.map {
      case Zero => '0'
      case One => '1'
    }
    Integer.parseUnsignedInt(bitChars.mkString, 2)
  }

}


object Day03bHelper {

  sealed trait Bit {
    def flipBit: Bit = this match {
      case Zero => One
      case One => Zero
    }
  }

  case object Zero extends Bit

  case object One extends Bit


  type DiagnosticBits = Seq[Bit]

  object DiagnosticBits {
    def apply(diagnosticBits: String): DiagnosticBits =
      diagnosticBits.map {
        case '0' => Zero
        case '1' => One
      }
  }


  final case class DiagnosticReport(diagnosticBitss: Seq[DiagnosticBits]) extends Uncopyable {
    def positions: Iterable[Int] = Range(0, diagnosticBitss.head.size)

    def getMostCommonBitAtPosition(pos: Int): Option[Bit] = {
      val bitsAtPos = diagnosticBitss.map(_(pos))
      val countOfOnes = bitsAtPos.count(_ == One)
      val compared = (countOfOnes * 2).compare(diagnosticBitss.size)
      if (compared > 0) Some(One)
      else if (compared < 0) Some(Zero)
      else None
    }
  }

}
