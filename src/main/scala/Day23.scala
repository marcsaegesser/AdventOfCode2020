package advent

object Day23 {

  def part1(cups: List[Int]): String = {
    val (a, b) = splitAt(moveN(100, cups), 1)
    (b ++ a.init).mkString
  }

  def findDest(c: Int, r: Set[Int], min: Int, max: Int): Int = {
    if(r.contains(c))  c
    else if(c-1 < min) max
    else               findDest(c-1, r, min, max)
  }

  def splitAt(cs: List[Int], d: Int): (List[Int], List[Int]) = {
    def helper(accum: List[Int], r: List[Int]): (List[Int], List[Int]) =
      r match {
        case h :: t if h == d => ((d :: accum).reverse, t)
        case h :: t => helper(h :: accum, t)
        case _      => throw new Exception("Invalid data")
      }

    helper(Nil, cs)
  }

  // @annotation.nowarn
  def move(cups: List[Int]): List[Int] = {
    val (h, t) = cups.splitAt(4)
    val cur = h.head
    val rem = t.toSet
    val dest = findDest(cur-1, rem, rem.min, rem.max)
    val (a, b) = splitAt(t, dest)

    (a ++ h.tail ++ b) :+ cur
  }

  def moveN(n: Int, cups: List[Int]): List[Int] =
    if(n == 0) cups
    else       moveN(n-1, move(cups))

  def parseInput(input: String): List[Int] =
    input.toList.map(_.asDigit)

  val testData = "389125467"

  val inputData = "198753462"
}
