// Taken from original AoC 2021 solutions, including most of the error handling parts.

import Day03aHelper._
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxUnorderedFoldableOps

import scala.util.{Failure, Success, Try}

object Day03a extends Puzzle[Try[DiagnosticReport], Try[Int]] {

  override def parseAFromLines(lines: Iterable[String]): Try[DiagnosticReport] = for {
    linesNel <- Try(NonEmptyList.fromListUnsafe(lines.toList))
    diagnosticBitsNel <- linesNel.traverse(DiagnosticBits(_))
    diagnosticReport <- DiagnosticReport(diagnosticBitsNel)
  } yield diagnosticReport

  override def solvePart1(diagnosticReportT: Try[DiagnosticReport]): Try[Int] = for {
    diagnosticReport <- diagnosticReportT
    positionsNel = diagnosticReport.positions
    gammaBits <- positionsNel.traverse(pos => getMostCommonBitAtPositionNoTies(diagnosticReport, pos))
    epsilonBits <- positionsNel.traverse(pos => getLeastCommonBitAtPositionNoTies(diagnosticReport, pos))
    gamma = valueOf(gammaBits)
    epsilon = valueOf(epsilonBits)
  } yield gamma * epsilon

  override def solvePart2(diagnosticReportT: Try[DiagnosticReport]): Try[Int] = for {
    diagnosticReport <- diagnosticReportT
    oxygenGeneratorRatingBits <- filterForMostCommonBitWithDefaultOne(diagnosticReport)
    co2ScrubberRatingBits <- filterForLeastCommonBitWithDefaultZero(diagnosticReport)
    oxygenGeneratorRating = valueOf(oxygenGeneratorRatingBits.diagnosticBits)
    co2ScrubberRating = valueOf(co2ScrubberRatingBits.diagnosticBits)
  } yield oxygenGeneratorRating * co2ScrubberRating

  private def filterForMostCommonBitWithDefaultOne(diagnosticReport: DiagnosticReport): Try[DiagnosticBits] =
    filterToSingleValue(diagnosticReport, 0, getMostCommonBitAtPositionWithDefaultOne)

  private def filterForLeastCommonBitWithDefaultZero(diagnosticReport: DiagnosticReport): Try[DiagnosticBits] =
    filterToSingleValue(diagnosticReport, 0, getLeastCommonBitAtPositionWithDefaultZero)

  //@tailrec // TODO find a tailrec solution
  def filterToSingleValue(diagnosticReport: DiagnosticReport,
                          pos: Int,
                          getRelevantBit: (DiagnosticReport, Int) => Try[Bit]): Try[DiagnosticBits] =
    diagnosticReport.diagnosticBitsNel match {
      case NonEmptyList(head, Nil) => Success(head)
      case diagnosticBitsNel => for {
        relevantBitAtPos <- getRelevantBit(diagnosticReport, pos)
        filteredDiagnosticBitsList = diagnosticBitsNel.collect {
          case diagnosticBits if diagnosticBits.getBit(pos) == Success(relevantBitAtPos) => diagnosticBits
        }
        _ = assume(filteredDiagnosticBitsList.nonEmpty) // as we filtered on the relevant bit, thus min. 1 entry exists
        filteredDiagnosticBitsNel = NonEmptyList.fromListUnsafe(filteredDiagnosticBitsList)
        filteredDiagnosticReport <- DiagnosticReport(filteredDiagnosticBitsNel)
        result <- filterToSingleValue(filteredDiagnosticReport, pos + 1, getRelevantBit)
      } yield result
    }

  private def getMostCommonBitAtPosition(diagnosticReport: DiagnosticReport, pos: Int, tiesResult: Try[Bit]): Try[Bit] =
    diagnosticReport.getMostCommonBitAtPosition(pos) match {
      case Success(Some(bit)) => Success(bit)
      case Success(None) => tiesResult
      case Failure(exception) => Failure(exception)
    }

  private def getMostCommonBitAtPositionNoTies(diagnosticReport: DiagnosticReport, pos: Int): Try[Bit] =
    getMostCommonBitAtPosition(diagnosticReport, pos, Failure(new IllegalStateException("No ties expected!")))

  private def getLeastCommonBitAtPositionNoTies(diagnosticReport: DiagnosticReport, pos: Int): Try[Bit] =
    getMostCommonBitAtPositionNoTies(diagnosticReport, pos).map(_.flipBit)

  private def getMostCommonBitAtPositionWithDefaultOne(diagnosticReport: DiagnosticReport, pos: Int): Try[Bit] =
    getMostCommonBitAtPosition(diagnosticReport, pos, Success(One))

  private def getLeastCommonBitAtPositionWithDefaultZero(diagnosticReport: DiagnosticReport, pos: Int): Try[Bit] =
    getMostCommonBitAtPositionWithDefaultOne(diagnosticReport, pos).map(_.flipBit)

  private def valueOf(bits: NonEmptyList[Bit]): Int = {
    val bitChars = bits.map {
      case Zero => '0'
      case One => '1'
    }
    Integer.parseUnsignedInt(bitChars.toList.mkString, 2)
  }

}


object Day03aHelper {

  sealed trait Bit {
    def flipBit: Bit = this match {
      case Zero => One
      case One => Zero
    }
  }

  case object Zero extends Bit

  case object One extends Bit


  final case class DiagnosticBits private(diagnosticBits: NonEmptyList[Bit]) extends Uncopyable {
    def length: Int = diagnosticBits.length

    def getBit(pos: Int): Try[Bit] = Try(diagnosticBits.toList(pos))
  }

  object DiagnosticBits {
    def apply(diagnosticBits: String): Try[DiagnosticBits] = for {
      nelBitChars <- Try(NonEmptyList.fromListUnsafe(diagnosticBits.toList))
      nelBits <- nelBitChars.traverse {
        case '0' => Success(Zero)
        case '1' => Success(One)
        case _ => Failure(new IllegalArgumentException("Only '0' or '1' are allowed bit values!"))
      }
    } yield DiagnosticBits(nelBits)
  }


  final case class DiagnosticReport private(diagnosticBitsNel: NonEmptyList[DiagnosticBits]) extends Uncopyable {
    private lazy val positionsRange = diagnosticBitsNel.head.diagnosticBits.toList.indices

    lazy val positions: NonEmptyList[Int] = NonEmptyList.fromListUnsafe(positionsRange.toList) // safe as nel

    private def getBitsAtPos(pos: Int): Try[NonEmptyList[Bit]] = diagnosticBitsNel.traverse(_.getBit(pos))

    def getMostCommonBitAtPosition(pos: Int): Try[Option[Bit]] = for {
      bitsAtPos <- getBitsAtPos(pos)
      countOfOnes = bitsAtPos.count(_ == One)
      compared = (countOfOnes * 2).compare(diagnosticBitsNel.length.toLong)
      mostCommonBitOpt = if (compared > 0) Some(One)
      else if (compared < 0) Some(Zero)
      else None
    } yield mostCommonBitOpt
  }

  object DiagnosticReport {
    def apply(diagnosticBitsNel: NonEmptyList[DiagnosticBits]): Try[DiagnosticReport] = {
      val headLength = diagnosticBitsNel.head.length
      val checked = diagnosticBitsNel.traverse(diagnosticBits => Try(require(diagnosticBits.length == headLength)))
      checked.map(_ => new DiagnosticReport(diagnosticBitsNel))
    }
  }
}
