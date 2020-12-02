package advent

object Day02 {

  def run(): Unit = {
    val entries = parseFile(inputFile)
    println(s"Day02.part1 = ${part1(entries)}")
    println(s"Day02.part2 = ${part2(entries)}")
  }

  def part1(entries: List[PasswordEntry]): Int =
    entries.filter(isValidPart1).size

  def part2(entries: List[PasswordEntry]): Int =
    entries.filter(isValidPart2).size

  case class PasswordEntry(first: Int, second: Int, c: Char, pwd: String)

  def isValidPart1(entry: PasswordEntry): Boolean = {
    val occurs = entry.pwd.filter(_ == entry.c).size
    occurs >= entry.first && occurs <= entry.second
  }

  def isValidPart2(entry: PasswordEntry): Boolean = {
    val f = entry.pwd(entry.first-1)
    val s = entry.pwd(entry.second-1)

    (f == entry.c || s == entry.c) && f != s
  }

  val PwdRegex = """(\d+)-(\d+)\s*(\w):\s*(\w+).*""".r

  def parseFile(file: String): List[PasswordEntry] =
    parseEntries(io.Source.fromFile(file).getLines())

  def parseEntries(entries: Iterator[String]): List[PasswordEntry] =
    entries.map(parseEntry).toList

  @scala.annotation.nowarn
  def parseEntry(entry: String): PasswordEntry = {
    val PwdRegex(first, second, c, pwd) = entry
    PasswordEntry(first.toInt, second.toInt, c.head, pwd)
  }

  val inputFile = "data/Day02.txt"

  def testData =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc""".stripMargin.linesIterator
}
