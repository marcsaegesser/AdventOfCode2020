package advent

object Day14 {
  def run(): Unit = {
    println(s"Day14.part1 = ${Day14a.run()}")
    println(s"Day14.part2 = ${Day14b.run()}")
  }
}

object Day14b {

  def run(): BigInt = part2(readFile(inputFile))

  def part2(input: List[Instruction]): BigInt =
    run(input).mem.values.sum

  case class State(mask: Mask, mem: Map[BigInt, BigInt])

  def run(instructions: List[Instruction]): State = {
    def helper(state: State, is: List[Instruction]): State =
      is match {
        case Nil    => state
        case h :: t => helper(step(state, h), t)
      }

    helper(State(Mask(List.empty, List.empty), Map.empty[BigInt, BigInt]), instructions)
  }

  def step(state: State, i: Instruction): State = {
    i match {
      case m@Mask(_, _) => state.copy(mask=m)
      case Mem(a, v)    => state.copy(mem=applyMask(state.mask, a).foldLeft(state.mem) { case (accum, a) => accum.updated(a, v) })
    }
  }

  def applyMask(mask: Mask, v: BigInt): List[BigInt] = {
    def helper(accum: List[BigInt], ops: List[FloatBit]): List[BigInt] =
      ops match {
        case Nil              => accum
        case FloatBit(i) :: t => helper( accum.map { x => x.flipBit(i) } ++ accum, t)
      }

    val set = mask.set.foldLeft(v) { case (a, SetBit(i)) => a.setBit(i) }
    helper(List(set), mask.float)
  }

  sealed trait MaskOp
  case class SetBit(b: Int) extends MaskOp
  case class FloatBit(b: Int) extends MaskOp

  sealed trait Instruction
  case class Mask(set: List[SetBit], float: List[FloatBit]) extends Instruction
  case class Mem(a: BigInt, v: BigInt)   extends Instruction

  def parseMask(m: String): Mask = {
    val (ss, fs) =
      m.reverse.zipWithIndex.foldLeft((List.empty[SetBit], List.empty[FloatBit])) {
        case ((ss, fs), ('0', _)) => (ss, fs)
        case ((ss, fs), ('1', i)) => (SetBit(i) :: ss, fs)
        case ((ss, fs), ('X', i)) => (ss, FloatBit(i) :: fs)
        case (_)            => throw new Exception("Invalid mask")
      }
    Mask(ss, fs)
  }

  val maskRegex = """mask\s=\s([X01]+)""".r
  val memRegex  = """mem\[(\d+)\]\s=\s(\d+)""".r

  def parseInstruction(input: String): Instruction =
    input match {
      case maskRegex(m)   => parseMask(m)
      case memRegex(a, v) => Mem(BigInt(a), BigInt(v))
      case _              => throw new Exception("Invalid instruction")
    }

  def parseInstructions(lines: Iterator[String]): List[Instruction] =
    lines.map(parseInstruction).toList


  def readFile(f: String): List[Instruction] =
    parseInstructions(io.Source.fromFile(f).getLines())

  def testData = """mask = 000000000000000000000000000000X1001X
                   |mem[42] = 100
                   |mask = 00000000000000000000000000000000X0XX
                   |mem[26] = 1""".stripMargin.linesIterator

  val inputFile = "data/Day14.txt"

}

object Day14a {
  def run(): BigInt = part1(readFile(inputFile))

  def part1(input: List[Instruction]): BigInt =
    run(input).mem.values.sum


  case class State(mask: Mask, mem: Map[Long, Long])

  def run(instructions: List[Instruction]): State = {
    def helper(state: State, is: List[Instruction]): State =
      is match {
        case Nil => state
        case h :: t => helper(step(state, h), t)
      }

    helper(State(Mask(0, 0), Map.empty[Long, Long]), instructions)
  }

  def step(state: State, i: Instruction): State =
    i match {
      case m@Mask(_, _) => state.copy(mask=m)
      case Mem(a, v)    => state.copy(mem=state.mem.updated(a, applyMask(state.mask, v)))
    }

  def applyMask(mask: Mask, v: Long): Long =
    v & mask.clear | mask.set

  sealed trait Instruction
  case class Mask(set: Long, clear: Long) extends Instruction
  case class Mem(addr: Long, value: Long) extends Instruction

  def parseMask(m: String): Mask = {
    val (s, c) =
      m.foldLeft((0L, 0L)) {
        case ((s, c), 'X') => (s << 1, (c << 1) ^ 0x01)
        case ((s, c), '0') => (s << 1, (c << 1))
        case ((s, c), '1') => ((s << 1) ^ 0x01, c << 0x01)
        case _             => throw new Exception("Invalid input")
      }
    Mask(s, c)
  }

  val maskRegex = """mask\s=\s([X01]+)""".r
  val memRegex  = """mem\[(\d+)\]\s=\s(\d+)""".r

  def parseInstruction(s: String): Instruction = {
    s match {
      case maskRegex(m)   => parseMask(m)
      case memRegex(a, v) => Mem(a.toLong, v.toLong)
      case _              => throw new Exception("Invalid instruction")
    }
  }

  def parseInstructions(lines: Iterator[String]): List[Instruction] =
    lines.map(parseInstruction).toList

  def readFile(f: String): List[Instruction] =
    parseInstructions(io.Source.fromFile(f).getLines())

  def testData = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                   |mem[8] = 11
                   |mem[7] = 101
                   |mem[8] = 0""".stripMargin.linesIterator

  val inputFile = "data/Day14.txt"

}


