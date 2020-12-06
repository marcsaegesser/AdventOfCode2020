package advent

object Day06 {

  type Answers = Set[Char]

  def part1(input: List[List[Answers]]): Int =
    input.map(_.reduce(_++_).size).sum

  def part2(input: List[List[Answers]]): Int =
    input.map(_.reduce(_&_).size).sum

  def parseInput(lines: Iterator[String]): List[List[Answers]] = {
    val (ll, l) =
      lines.foldLeft((List.empty[List[Answers]], List.empty[Answers])) { case ((ll, l), s) =>
        if(s.trim.isEmpty) (l +: ll, List.empty[Answers])
        else               (ll, s.toSet +: l)

      }

    l +: ll
  }

  def readFile(f: String): List[List[Answers]] =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day06.txt"

  def testData = """abc
                   |
                   |a
                   |b
                   |c
                   |
                   |ab
                   |ac
                   |
                   |a
                   |a
                   |a
                   |a
                   |
                   |b""".stripMargin.linesIterator
}
