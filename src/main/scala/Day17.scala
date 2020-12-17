package advent

object Day17 {
  def run(): Unit = {
    println(s"Day17.part1 = ${Day17_1.part1()}")
    println(s"Day17.part2 = ${Day17_2.part2()}")
  }
}

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
