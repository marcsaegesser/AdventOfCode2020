package advent

object Day15 {

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
