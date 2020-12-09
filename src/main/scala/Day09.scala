package advent

object Day09 {

  def run(): Unit = {
    val input = readFile(inputFile)
    val error = findError(input, 25)
    val weakness = findWeakness(input, error)
    println(s"Day09.part1 = $error")
    println(s"Day09.part2 = $weakness")
  }

  def findError(input: List[Long], size: Int): Long = {
    def rule(l: List[Long], x: Long) =
      l.combinations(2).find(_.sum == x) match {   // It seems like there should be a combinator for this already
        case Some(_) => None
        case None    => Some(x)
      }

    def helper(l: List[Long]): Option[Long] =
      if(l.size < size+1) None
      else rule(l.take(size), l.drop(size).head).orElse(helper(l.tail))

    helper(input).getOrElse(throw new Exception("Invalid input"))
  }

  def findWeakness(input: List[Long], target: Long): Long = {
    def helper(n: Int): Long = {
      input.sliding(n).find(_.sum == target) match {
        case Some(x) => x.min + x.max
        case None    => helper(n+1)
      }
    }

    helper(2)
  }

  def parseInput(s: Iterator[String]): List[Long] =
    s.map(_.toLong).toList

  def readFile(f: String): List[Long] =
    io.Source.fromFile(f)
      .getLines()
      .map(_.toLong)
      .toList

  val inputFile = "data/Day09.txt"

  def testData = """35
                   |20
                   |15
                   |25
                   |47
                   |40
                   |62
                   |55
                   |65
                   |95
                   |102
                   |117
                   |150
                   |182
                   |127
                   |219
                   |299
                   |277
                   |309
                   |576
                   |""".stripMargin.linesIterator
}
