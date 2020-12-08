package advent

object Day08 {

  def run(): Unit = {
    val state = readFile(inputFile)
    println(s"Day08.part1 = ${part1(state)}")
    println(s"Day08.part2 = ${part2(state)}")
  }

  def part1(state: MachineState): Int =
    runUntilRepeat(state).acc

  def part2(state: MachineState): Int =
    programsFrom(state)
      .map(run)
      .collectFirst { case Some(s) => s.acc }
      .get

  case class MachineState(ip: Int, acc: Int, instructions: Vector[Instruction], history: Set[Int])

  def programsFrom(initial: MachineState): LazyList[MachineState] =
    LazyList.from(0).zip(initial.instructions).flatMap { case (ip, i) =>
      i match {
        case NOP(a) => Some(initial.copy(instructions=initial.instructions.updated(ip, JMP(a))))
        case JMP(a) => Some(initial.copy(instructions=initial.instructions.updated(ip, NOP(a))))
        case ACC(_) => None
      }
    }

  def runUntilRepeat(state: MachineState): MachineState =
    if(state.history.contains(state.ip)) state
    else                                 runUntilRepeat(step(state))

  def run(state: MachineState): Option[MachineState] =
    if(state.history.contains(state.ip))         None
    else if(state.ip == state.instructions.size) Some(state)
    else                                         run(step(state))

  def step(state: MachineState): MachineState =
    state match { case MachineState(ip, acc, is, h) =>
      is(ip) match {
        case NOP(_) => state.copy(ip=ip+1, history=h+ip)
        case ACC(a) => state.copy(ip=ip+1, history=h+ip, acc=acc+a)
        case JMP(a) => state.copy(ip=ip+a, history=h+ip)
      }
    }

  sealed trait Instruction
  case class NOP(arg: Int) extends Instruction
  case class ACC(arg: Int) extends Instruction
  case class JMP(arg: Int) extends Instruction

  def parseInstruction(s: String): Instruction =
    s.split(" ") match {
      case Array("nop", a) => NOP(a.toInt)
      case Array("acc", a) => ACC(a.toInt)
      case Array("jmp", a) => JMP(a.toInt)
      case u               => throw new Exception(s"Unknown instruction $u")
    }

  def parseInput(input: Iterator[String]): MachineState =
    MachineState(0, 0, input.map(parseInstruction).toVector, Set.empty[Int])

  def readFile(f: String): MachineState =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day08.txt"

  def testData = """nop +0
                   |acc +1
                   |jmp +4
                   |acc +3
                   |jmp -3
                   |acc -99
                   |acc +1
                   |jmp -4
                   |acc +6""".stripMargin.linesIterator
}
