package advent

// Turns out Vector is much faster than Map.
object Day15 {

  def run(): Unit = {
    println(s"Day15.part1 = ${part1(inputData)}")
    println(s"Day15.part2 = ${part2(inputData)}")
  }

  def part1(input: String): Int =
    playN(2020, input)

  def part2(input: String): Int =
    playN(30000000, input)

  def playN(n: Int, input: String) = {
    def helper(t: Int, l: Int, s: Vector[Int]): Int =
      if(t == n)         l
      else if(s(l) == 0) helper(t+1, 0, s.updated(l, t))
      else               helper(t+1, t - s(l), s.updated(l, t))

    val (t, l, s) = parseInput(n, input)
    helper(t, l, s)
  }

  def parseInput(n: Int, input: String): (Int, Int, Vector[Int]) = {
    val l =
      input.split(",")
        .toList
        .map(_.toInt)
        .zip(LazyList.from(1))
        .reverse
    (l.size, l.head._1, l.tail.foldLeft(Vector.fill(n)(0)) { case (v, (i, t)) =>  v.updated(i, t)})
  }

  case class State(turn: Int, last: Int, seen: Vector[Int])
  val testData = "0,3,6"

  val inputData = "0,3,1,6,7,5"
}

object Day15a {

  def part1(input: State): Int =
    LazyList.iterate(input)(next).dropWhile(_.turn < 2020).head.last

  // This is horribly slow and requires additional heap space
  // Come back to find a better solution
  def part2(input: State): Int =
    LazyList.iterate(input)(next).dropWhile(_.turn < 30000000).head.last


  @annotation.tailrec
  def playN(n: Int, state: State): State = {
    if(state.turn == n) state
    else                playN(n, next(state))
  }

  def next(state: State): State =
    state match { case State(t, l, h) =>
      h.get(l) match {
        case None    => State(t+1, 0,     h.updated(l, t))
        case Some(x) => State(t+1, t - x, h.updated(l, t))
      }
    }

  case class State(turn: Int, last: Int, history: Map[Int, Int])

  def parseInput(input: String): State = {
    val l =
      input.split(",")
        .toList
        .map(_.toInt)
        .zip(LazyList.from(1))
        .reverse
    State(l.size, l.head._1, l.tail.toMap)
  }

  val testData = "0,3,6"

  val inputData = "0,3,1,6,7,5"
}
