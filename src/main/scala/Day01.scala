package advent

object Day01 {

  def run(): Unit = {
    val input = readFile(inputFile)
    println(s"Day01.part1 = ${part1(input)}")
    println(s"Day01.part2 = ${part2(input)}")
  }

  def part1(input: List[Int]): Int =
    findSum2020_2(input).map(_.product).getOrElse(throw new Exception("No match found"))

  def part2(input: List[Int]): Int =
    findSum2020_3(input).map(_.product).getOrElse(throw new Exception("No match found"))

  def findSum2020_2(input: List[Int]) =
    input.combinations(2).find(_.sum == 2020)

  def findSum2020_3(input: List[Int]) =
    input.combinations(3).find(_.sum == 2020)

  def parseInput(input: Iterator[String]): List[Int] =
    input.map(_.toInt).toList

  def readFile(file: String): List[Int] =
    parseInput(io.Source.fromFile(file).getLines())

  val inputFile = "data/Day01.txt"

  val testData = """1721
                   |979
                   |366
                   |299
                   |675
                   |1456""".stripMargin.linesIterator
}
