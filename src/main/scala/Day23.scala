package advent

object Day23 {

  def run(): Unit = {
    println(s"Day23.part1 = ${part1(inputData)}")
    println(s"Day23.part2 = ${part2(inputData)}")
  }

  def part1(input: String): String =
    Iterator
      .iterate(mkGame(input, 0))(move)
      .drop(100).next()
      .cupsFrom(1, 9).tail.mkString

  def part2(input: String): Long =
    Iterator.iterate(mkGame(input, 1000000))(move)
      .drop(10000000).next()
      .cupsFrom(1, 3).tail
      .map(_.toLong)
      .product


  /*
   * I wish I could take credit for this idea. The cups value is a map
   * from a cup number to its successor cup number. This allows the
   * game to be played 'in place'. This solution is based on James Constable's
   * solution (https://github.com/jamesconstable)
   *
   * My initial thinking was to approach this like last year's
   * permutation problem but I couldn't see a way to de-functionalize
   * this puzzle
   */
  case class Game(curr: Int, cups: Map[Int, Int]) {
    def cupsFrom(start: Int, n: Int): List[Int] =
      LazyList.iterate(start)(cups.apply).take(n).toList
  }

  @annotation.nowarn                 // The LazyList pattern match is not exhaustive, but we know this is safe
  def move(game: Game): Game = {
    def wrap(i: Int): Int = if(i <= 0) i + game.cups.size else i
    game match { case Game(curr, cups) =>
      val _ #:: r1 #:: r2 #:: r3 #:: next #:: _ = LazyList.iterate(curr)(cups.apply)
      val dest = LazyList.from(1).map(n => wrap(curr - n)).filterNot(Set(r1, r2, r3).contains(_)).head
      Game(next,
        cups
          .updated(r3, cups(dest))   // Splice r1 r2 r3 between dest's successor
          .updated(dest, r1)         //    and dest
          .updated(curr, next)       // Point curr to next to remove the cups from their current location
      )
    }
  }

  def mkGame(input: String, length: Int): Game ={
    val nums = input.toList.map(_.asDigit)    // The input digits
    val cs = nums ++ (nums.max+1 to length)   // Pad to length, if neccessary

    val cups = cs.zip(LazyList.continually(cs).flatten.drop(1)).toMap
    Game(cs.head, cups)
  }

  val testData = "389125467"

  val inputData = "198753462"
}

object Day23a {

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

  def moveNp(n: Int, cups: List[Int]): List[Int] ={
    println(s"$n: $cups")
    if(n == 0) cups
    else       moveNp(n-1, move(cups))
  }

  def parseInput(input: String): List[Int] =
    input.toList.map(_.asDigit)

  val testData = "389125467"

  val inputData = "198753462"
}
