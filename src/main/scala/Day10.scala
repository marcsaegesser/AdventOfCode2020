package advent

object Day10 {

  def run(): Unit = {
    val input = readFile(inputFile)
    println(s"Day10.part1 = ${part1(input)}")
    println(s"Day10.part2 = ${part2(input)}")
  }

  def part1(input: List[Int]) = {
    val (a, b) =
      diffs(input)
        .foldLeft((0, 0)) {
          case ((a, b), 1) => (a+1, b)
          case ((a, b), 2) => (a, b)
          case ((a, b), 3) => (a, b+1)
          case _           => throw new Exception("Invalid input")
        }
    a*b
  }

  def part2(input: List[Int]): Long =
    arrangements(input)._2.head

  def diffs(input: List[Int]): List[Int] =
    input.zip(input.tail).map { case (a, b) => b-a }


  // I'll admit defeat and that I had to look this one up
  def arrangements(input: List[Int]) = {
    input.foldRight((List.empty[Int], List.empty[Long])) { case (x, (xs, res)) =>
      val nextRes =
        xs
          .takeWhile(_ <= x + 3)
          .zip(res)
          .map(_._2)
          .sum
      (x :: xs, Math.max(1, nextRes) :: res)
    }
  }

  def parseInput(input: Iterator[String]): List[Int] = {
    val i = input.map(_.toInt).toList
    (List(0, i.max+3) ++ i).sorted
  }


  def readFile(f: String): List[Int] =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day10.txt"

  def testData1 = """16
                    |10
                    |15
                    |5
                    |1
                    |11
                    |7
                    |19
                    |6
                    |12
                    |4""".stripMargin.linesIterator

  def testData3 = """16
                    |10
                    |15
                    |5
                    |1
                    |11
                    |8
                    |7
                    |19
                    |6
                    |12
                    |4""".stripMargin.linesIterator

  def testData2 = """28
                    |33
                    |18
                    |42
                    |31
                    |14
                    |46
                    |20
                    |48
                    |47
                    |24
                    |23
                    |49
                    |45
                    |19
                    |38
                    |39
                    |11
                    |1
                    |32
                    |25
                    |35
                    |8
                    |17
                    |7
                    |9
                    |4
                    |2
                    |34
                    |10
                    |3""".stripMargin.linesIterator
}
