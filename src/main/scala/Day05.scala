package advent

object Day05 {

  def run(): Unit = {
    val input = readFile(inputFile)
    println(s"Day05.part1 = ${part1(input)}")
    println(s"Day05.part2 = ${part2(input)}")
  }

  def part1(input: List[Operations]): Int =
    input.map(os => computeSeatId(runOperations(initialState, os))).max

  def part2(input: List[Operations]): Int = {
    val seatIds = input.map(os => computeSeatId(runOperations(initialState, os))).sorted
    LazyList.from(seatIds.head)
      .zip(seatIds)
      .find { case (a, b) => a != b }
      .get._1  // Yeah, not proud
  }


  type Pair = (Int, Int)
  case class State(rowPair: Pair, colPair: Pair)
  val initialState = State((0, 127), (0, 7))

  def runOperations(state: State, operations: Operations): State = {
    operations match {
      case Nil    => state
      case h :: t => runOperations(runOperation(state, h), t)
    }
  }

  def runOperation(state: State, op: Operation): State = {
    def update(min: Int, max: Int): Int = (max - min + 1) / 2 + min
    op match {
      case 'F' => state.copy(rowPair = (state.rowPair._1, update(state.rowPair._1, state.rowPair._2)-1))
      case 'B' => state.copy(rowPair = (update(state.rowPair._1, state.rowPair._2), state.rowPair._2))
      case 'L' => state.copy(colPair = (state.colPair._1, update(state.colPair._1, state.colPair._2)-1))
      case 'R' => state.copy(colPair = (update(state.colPair._1, state.colPair._2), state.colPair._2))
      case o   => throw new Exception(s"Invalid operation $o")
    }
  }

  def computeSeatId(state: State): Int =
    state.rowPair._1 * 8 + state.colPair._1

  type Operation  = Char
  type Operations = List[Operation]

  def parseInput(lines: Iterator[String]): List[Operations] =
    lines.map(_.toList).toList

  def readFile(f: String): List[Operations] =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day05.txt"

  def testData = """FBFBBFFRLR
                   |BFFFBBFRRR
                   |FFFBBBFRRR
                   |BBFFBBFRLL""".stripMargin.linesIterator

  val testData1 = "FBFBBFFRLR"
  val row1 = 44
  val col1 = 5
  val id1 = 357

  val testData2 = "BFFFBBFRRR"
  val row2 = 70
  val col2 = 7
  val id2 = 567

  val testData3 = "FFFBBBFRRR"
  val row3 = 14
  val col3 = 7
  val id3 = 119

  val testData4 = "BBFFBBFRLL"
  val row4 = 102
  val col4 = 4
  val id4 = 820
}
