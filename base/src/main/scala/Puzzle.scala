trait Puzzle[A, B] {

  def parseAFromLines(lines: Iterable[String]): A

  def solvePart1(a: A): B

  def solvePart2(a: A): B

  final def solvePart1(lines: Iterable[String]): B = solvePart1(parseAFromLines(lines))

  final def solvePart2(lines: Iterable[String]): B = solvePart2(parseAFromLines(lines))

}
