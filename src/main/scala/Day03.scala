package advent

object Day03 {

  def run(): Unit = {
    val trees = parseMap(readFile(inputFile))
    println(s"Day03.part1 = ${part1(trees)}")
    println(s"Day03.part2 = ${part2(trees)}")
  }

  def part1(trees: Trees): Long = {
    checkSlope(trees, Coord(3, 1))
  }

  def part2(trees: Trees): Long = {
    val slopes = List(Coord(1, 1), Coord(3, 1), Coord(5, 1), Coord(7, 1), Coord(1, 2))
    slopes.map(checkSlope(trees, _)).product
  }

  def checkSlope(trees: Trees, slope: Coord): Long = {
    def helper(accum: Long, pos: Coord): Long =
      if(pos.y > trees.maxY)      accum
      else if(isTree(trees, pos)) helper(accum+1, Coord(pos.x+slope.x, pos.y+slope.y))
      else                        helper(accum,   Coord(pos.x+slope.x, pos.y+slope.y))

    helper(0L, Coord(0, 0))
  }

  def isTree(trees: Trees, c: Coord): Boolean = {
    c match { case Coord(x, y) =>
      trees.map(Coord(x % (trees.maxX+1), y))
    }
  }

  case class Coord(x: Int, y: Int)
  case class Trees(map: Set[Coord], maxX: Int, maxY: Int)
  final val Empty = '.'
  final val Tree  = '#'

  def parseLine(y: Int, line: String): Set[Coord] =
    LazyList.from(0).zip(line).foldLeft(Set.empty[Coord]) { case (a, (x, t)) =>
      if(t == Tree) a + Coord(x, y)
      else          a
    }

  def extent(map: Set[Coord]): (Int, Int) =
    (map.maxBy(_.x).x, map.maxBy(_.y).y)

  def parseMap(input: Iterator[String]): Trees = {
    val map =
      LazyList.from(0)
        .zip(input)
        .flatMap { case (y, l) => parseLine(y, l) }
        .toSet

    val (maxX, maxY) = extent(map)
    Trees(map, maxX, maxY)
  }

  def readFile(f: String): Iterator[String] =
    io.Source.fromFile(f).getLines()


  val inputFile = "data/Day03.txt"

  def testData = """..##.......
                   |#...#...#..
                   |.#....#..#.
                   |..#.#...#.#
                   |.#...##..#.
                   |..#.##.....
                   |.#.#.#....#
                   |.#........#
                   |#.##...#...
                   |#...##....#
                   |.#..#...#.#""".stripMargin.linesIterator
}
