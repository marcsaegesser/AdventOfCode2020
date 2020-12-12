package advent

object Day12 {

  def run(): Unit = {
    val moves = readFile(inputFile)
    println(s"Day12.part1 = ${part1(moves)}")
    println(s"Day12.part2 = ${part2(moves)}")
  }

  def part1(moves: Movements): Int =
    run(initialState, moves) match {
      case State(x, y, _) => distance(x, y)
    }

  def part2(moves: Movements): Int =
    run2(initialState2, moves) match {
      case State2(x, y, _) => distance(x, y)
    }

  case class State2(x: Int, y: Int, wpt: (Int, Int))
  val initialState2 = State2(0, 0, (10, 1))

  def run2(state: State2, ms: Movements): State2 =
    ms match {
      case Nil    => state
      case h :: t => run2(applyMovement2(state, h), t)
    }

  def applyMovement2(state: State2, m: Movement): State2 =
    m match {
      case N(a) => state.copy(wpt=(state.wpt._1,   state.wpt._2+a))
      case S(a) => state.copy(wpt=(state.wpt._1,   state.wpt._2-a))
      case E(a) => state.copy(wpt=(state.wpt._1+a, state.wpt._2))
      case W(a) => state.copy(wpt=(state.wpt._1-a, state.wpt._2))
      case F(a) => state.copy(x=state.x+a*state.wpt._1, y=state.y+a*state.wpt._2)
      case L(a) => state.copy(wpt=rotate(state.wpt, a))
      case R(a) => state.copy(wpt=rotate(state.wpt, -a))
    }

  def rotate(wpt: (Int, Int), amount: Int): (Int, Int) = {
    (amount/90 + 4)%4 match {
      case 0 => wpt
      case 1 => (-wpt._2,  wpt._1)
      case 2 => (-wpt._1, -wpt._2)
      case 3 => ( wpt._2, -wpt._1)
    }
  }

  def applyMovement(state: State, m: Movement): State =
    m match {
      case N(a) => state.copy(y=state.y+a)
      case S(a) => state.copy(y=state.y-a)
      case E(a) => state.copy(x=state.x+a)
      case W(a) => state.copy(x=state.x-a)
      case F(a) => state.copy(x=state.x+a*directions(state.dir)._1, y=state.y+a*directions(state.dir)._2)
      case L(a) => state.copy(dir=(state.dir + a/90 + 4)%4)
      case R(a) => state.copy(dir=(state.dir - a/90 + 4)%4)
    }

  def run(state: State, ms: Movements): State =
    ms match {
      case Nil    => state
      case h :: t => run(applyMovement(state, h), t)
    }

  val directions = Vector((1, 0), (0, 1), (-1, 0), (0, -1))

  def distance(x: Int, y: Int): Int = math.abs(x) + math.abs(y)


  case class State(x: Int, y: Int, dir: Int)
  val initialState = State(0, 0, 0)

  type Movements = List[Movement]

  sealed trait Movement { def amount: Int }
  case class N(amount: Int) extends Movement
  case class S(amount: Int) extends Movement
  case class E(amount: Int) extends Movement
  case class W(amount: Int) extends Movement
  case class F(amount: Int) extends Movement
  case class L(amount: Int) extends Movement
  case class R(amount: Int) extends Movement

  val movementRegex = """([NSEWFLR])(\d+)""".r
  def parseMovement(s: String): Movement =
    s match {
      case movementRegex("N", a) => N(a.toInt)
      case movementRegex("S", a) => S(a.toInt)
      case movementRegex("E", a) => E(a.toInt)
      case movementRegex("W", a) => W(a.toInt)
      case movementRegex("F", a) => F(a.toInt)
      case movementRegex("L", a) => L(a.toInt)
      case movementRegex("R", a) => R(a.toInt)
      case _ => throw new Exception("Invalid input")
    }

  def parseInput(lines: Iterator[String]): Movements =
    lines.map(parseMovement).toList

  def readFile(f: String): Movements =
    io.Source.fromFile(f)
      .getLines()
      .map(parseMovement)
      .toList

  val inputFile = "data/Day12.txt"

  def testData = """F10
                   |N3
                   |F7
                   |R90
                   |F11""".stripMargin.linesIterator
}
