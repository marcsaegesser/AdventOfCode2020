package advent

/** A general n-dimeninsional Conway Game of Live
  *
  * It's a little slower than the games for specific dimensions but
  * works for any n.
  */
object Day17 {
  def run(): Unit = {
    println(s"Day17.part1 = ${part1(readFile(3, inputFile))}")
    println(s"Day17.part2 = ${part2(readFile(4, inputFile))}")
  }

  type Space = Set[CoordN]

  case class CoordN(xs: Vector[Int]) {
    def apply(is: Iterable[Int]): CoordN = CoordN(is.toVector)

    def adjacent = {
      (0 until xs.size).foldLeft(Set(this)) { case (a, i) =>
        a.flatMap { x => (x.xs(i)-1 to x.xs(i)+1).map(xx => CoordN(x.xs.updated(i, xx))) }
      } - this
    }
  }

  def part1(space: Space): Int =
    runN(6, space).size

  def part2(space: Space): Int =
    runN(6, space).size

  def reachable(space: Space): Set[CoordN] =
    space.foldLeft(Set.empty[CoordN]) { case (a, c) => a ++ c.adjacent }

  def applyRules(c: CoordN, space: Space): Boolean = {
    (space(c), (c.adjacent & space).size) match {
      case (true, sz) if Set(2, 3).contains(sz) => true
      case (true, _)                            => false
      case (false, sz) if sz == 3               => true
      case (false, _)                           => false
    }
  }

  def step(space: Space): Space = {
    reachable(space).filter(applyRules(_, space))
  }

  def runN(n: Int, space: Space): Space =
    if(n == 0) space
    else       runN(n-1, step(space))

  def parseInput(dim: Int, lines: Iterator[String]): Space = {
    val pad = Vector.fill(dim - 2)(0)

    def parseLine(y: Int, line: String): Set[CoordN] =
      line.zipWithIndex.collect { case (c, x) if c == '#' => CoordN(Vector(x, y) ++ pad) }.toSet

    lines.zipWithIndex.foldLeft(Set.empty[CoordN]) { case (s, (l, y)) => s ++ parseLine(y, l) }
  }

  def readFile(dim: Int, f: String): Space =
    parseInput(dim, io.Source.fromFile(f).getLines())


  val inputFile = "data/Day17.txt"

  def testData = """.#.
                   |..#
                   |###""".stripMargin.linesIterator
}

// A 4 dimeninsional game of life
object Day17_2 {

  def part2(): Int = {
    runN(6, readFile(inputFile)).size
  }

  case class Coord(x: Int, y: Int, z: Int, w: Int)
  type Space = Set[Coord]

  def reachable(space: Space): Set[Coord] =
    space.foldLeft(Set.empty[Coord]) { case (a, c) => a ++ adjacent(c) }

  def applyRules(c: Coord, space: Space): Boolean = {
    (space(c), (adjacent(c) & space).size) match {
      case (true, sz) if Set(2, 3).contains(sz) => true
      case (true, _)                            => false
      case (false, sz) if sz == 3               => true
      case (false, _)                           => false
    }
  }

  def step(space: Space): Space = {
    reachable(space).filter(applyRules(_, space))
  }

  def runN(n: Int, space: Space): Space =
    if(n == 0) space
    else       runN(n-1, step(space))

  def adjacent(c: Coord): Set[Coord] =
    (for {
      xx <- (c.x-1 to c.x+1)
      yy <- (c.y-1 to c.y+1)
      zz <- (c.z-1 to c.z+1)
      ww <- (c.w-1 to c.w+1)
    } yield Coord(xx, yy, zz, ww)).toSet - c

  def parseInput(lines: Iterator[String]): Space = {
    def parseLine(y: Int, line: String): Set[Coord] =
      line.zipWithIndex.collect { case (c, x) if c == '#' => Coord(x, y, 0, 0) }.toSet

    lines.zipWithIndex.foldLeft(Set.empty[Coord]) { case (s, (l, y)) => s ++ parseLine(y, l) }
  }

  def readFile(f: String): Space =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day17.txt"

  def testData = """.#.
                   |..#
                   |###""".stripMargin.linesIterator
}

// A 3 dimeninsional game of life
object Day17_1 {

  def part1(): Int =
    runN(6, readFile(inputFile)).size

  case class Coord(x: Int, y: Int, z: Int)
  type Space = Set[Coord]

  def reachable(space: Space): Set[Coord] =
    space.foldLeft(Set.empty[Coord]) { case (a, c) => a ++ adjacent(c) }

  def applyRules(c: Coord, space: Space): Boolean = {
    (space(c), (adjacent(c) & space).size) match {
      case (true, sz) if Set(2, 3).contains(sz) => true
      case (true, _)                            => false
      case (false, sz) if sz == 3               => true
      case (false, _)                           => false
    }
  }

  def step(space: Space): Space = {
    reachable(space).filter(applyRules(_, space))
  }

  def runN(n: Int, space: Space): Space =
    if(n == 0) space
    else       runN(n-1, step(space))

  def adjacent(c: Coord): Set[Coord] =
    (for {
      xx <- (c.x-1 to c.x+1)
      yy <- (c.y-1 to c.y+1)
      zz <- (c.z-1 to c.z+1)
    } yield Coord(xx, yy, zz)).toSet - c

  def parseInput(lines: Iterator[String]): Space = {
    def parseLine(y: Int, line: String): Set[Coord] =
      line.zipWithIndex.collect { case (c, x) if c == '#' => Coord(x, y, 0) }.toSet

    lines.zipWithIndex.foldLeft(Set.empty[Coord]) { case (s, (l, y)) => s ++ parseLine(y, l) }
  }

  def readFile(f: String): Space =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day17.txt"

  def testData = """.#.
                   |..#
                   |###""".stripMargin.linesIterator
}
