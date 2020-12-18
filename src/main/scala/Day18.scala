package advent

object Day18 {

  def run(): Unit = {
    val input = readFile(inputFile)
    println(s"Day18.part1 = ${part1(input)}")
    println(s"Day18.part2 = ${part2(input)}")
  }

  def part1(input: List[String]): Long =
    input.map(s => eval(s, precedence1)).sum

  def part2(input: List[String]): Long =
    input.map(s => eval(s, precedence2)).sum

  // Quite possibly the worst parser ever
  sealed trait Expr
  case class Num(x: Long) extends Expr
  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Mult(lhs: Expr, rhs: Expr) extends Expr

  val precedence1: Precedence      = Map(Plus -> 0, Mult -> 0)
  val precedence2: Precedence      = Map(Plus -> 1, Mult -> 0)
  val precedenceNormal: Precedence = Map(Plus -> 0, Mult -> 1)

  type BinaryOp = (Expr, Expr) => Expr
  type Precedence = Map[BinaryOp, Int]

  case class State(args: List[Expr], ops: List[BinaryOp], precedence: Precedence) {
    def addArg(expr: Expr): State = this.copy(args=expr :: args)
    def addOp(op: BinaryOp): State = {
      (args, ops) match {
        case (a2 :: a1 :: as, o1 :: os) if precedence(op) <= precedence(o1) =>
          State(applyOp(o1, a1, a2) :: as, op :: os, precedence)
        case (as, os) => this.copy(ops=op :: os)
      }
    }

    def applyOp(op: BinaryOp, lhs: Expr, rhs: Expr): Expr =
      op(lhs, rhs)

    def reduce: Expr =
      (args, ops) match {
        case (Nil, _) => throw new Exception("Invalid state")
        case (a1 :: Nil, _) => a1
        case (a2 :: a1 :: as, op :: os) => State(args=applyOp(op, a1, a2) :: as, ops=os, precedence).reduce
        case (_, _) => throw new Exception("Invalid state")
      }

  }

  object State {
    def empty(precedence: Precedence): State = State(Nil, Nil, precedence)
  }

  def p(tokens: List[Char], state: State): (Expr, List[Char]) = {
    tokens match {
      case Nil      => (state.reduce, Nil)
      case ' ' :: r => p(r, state)
      case '(' :: r =>
        val (e, rr) = p(r, State.empty(state.precedence))
        p(rr, state.addArg(e))
      case ')' :: r => (state.reduce, r)
      case '+' :: r => p(r, state.addOp(Plus))
      case '*' :: r => p(r, state.addOp(Mult))
      case x   :: r => p(r, state.addArg(Num(x.asDigit.toLong)))
    }
  }

  def parse(s: String, precedence: Precedence): Expr = {
    p(s.toList, State.empty(precedence))._1
  }

  def eval(expr: Expr): Long =
    expr match {
      case Num(x)         => x
      case Mult(lhs, rhs) => eval(lhs) * eval(rhs)
      case Plus(lhs, rhs) => eval(lhs) + eval(rhs)
    }

  def eval(s: String, precedence: Precedence): Long =
    eval(parse(s, precedence))

  def readFile(f: String): List[String] =
    io.Source.fromFile(f).getLines().toList

  val inputFile = "data/Day18.txt"

  def testData = """1 + 2 * 3 + 4 * 5 + 6
                   |1 + (2 * 3) + (4 * (5 + 6))
                   |2 * 3 + (4 * 5)
                   |5 + (8 * 3 + 9 + 3 * 4 * 3)
                   |5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
                   |((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2""".stripMargin.linesIterator
}
