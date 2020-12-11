package advent

object Day11 {

  def run(): Unit = {
    val seats = readFile(inputFile)
    println(s"Day11.part1 = ${part1(seats)}")
    println(s"Day11.part2 = ${part2(seats)}")
  }

  def part1(seats: Seats): Int =
    run(seats).values.filter(_ == Taken).size

  def part2(seats: Seats): Int =
    run2(seats).values.filter(_ == Taken).size

  def run(seats: Seats): Seats = {
    val next = step(seats)
    if(next == seats) seats
    else              run(next)
  }

  def step(seats: Seats): Seats = {
    seats.map {
      case (pos, Empty) if countSurroundingOccupied(seats, pos) == 0 => (pos, Taken)
      case (pos, Taken) if countSurroundingOccupied(seats, pos) >= 4 => (pos, Empty)
      case (p, c)                                                    => (p, c)
    }
  }

  def run2(seats: Seats): Seats = {
    val visibility = computeAllVisibleSeats(seats)

    def helper(curr: Seats): Seats = {
      val next = step2(curr, visibility)
      if(next == curr) curr
      else             helper(next)
    }

    helper(seats)
  }

  def countVisibleOccupied(seats: Seats, pos: Coord, visibility: Visibility): Int =
    visibility(pos).flatMap(seats.get).filter(_ == Taken).size

  def step2(seats: Seats, visibility: Map[Coord, List[Coord]]): Seats =
    seats.map {
      case (pos, Empty) if countVisibleOccupied(seats, pos, visibility) == 0 => (pos, Taken)
      case (pos, Taken) if countVisibleOccupied(seats, pos, visibility) >= 5 => (pos, Empty)
      case (p, c)                                                       => (p, c)
    }

  def countSurroundingOccupied(seats: Seats, pos: Coord): Int =
    pos.adjacent.flatMap(seats.get).filter(_ == Taken).size

  def findVisibleFrom(seats: Seats, c: Coord) = {
    val (Coord(minX, minY), Coord(maxX, maxY)) = extent(seats)

    def inRegion(pos: Coord): Boolean = pos.x >= minX && pos.x <= maxX && pos.y >= minY && pos.y <= maxY

    def visible(pos: Coord, dir: Coord => Coord): Option[Coord] = {
      val next = dir(pos)
      if(!inRegion(next)) None
      else
        seats.get(next) match {
          case Some(_) => Some(next)
          case None    => visible(next, dir)
        }

    }

    List(u(_), d(_), l(_), r(_), ur(_), ul(_), dr(_), dl(_))
      .flatMap {d => visible(c, d)}
  }

  def computeAllVisibleSeats(seats: Seats): Visibility = {
    seats.keySet.map(c => (c, findVisibleFrom(seats, c))).toMap
  }


  sealed trait Seat
  case object Empty extends Seat
  case object Taken extends Seat

  type Seats = Map[Coord, Seat]
  type Visibility = Map[Coord, List[Coord]]

  def u(c: Coord) = c.u
  def d(c: Coord) = c.d
  def l(c: Coord) = c.l
  def r(c: Coord) = c.r
  def ur(c: Coord) = c.ur
  def ul(c: Coord) = c.ul
  def dr(c: Coord) = c.dr
  def dl(c: Coord) = c.dl

  case class Coord(x: Int, y: Int) {
    def u: Coord = Coord(x, y-1)
    def d: Coord = Coord(x, y+1)
    def l: Coord = Coord(x-1, y)
    def r: Coord = Coord(x+1, y)
    def ur: Coord = Coord(x+1, y-1)
    def ul: Coord = Coord(x-1, y-1)
    def dr: Coord = Coord(x+1, y+1)
    def dl: Coord = Coord(x-1, y+1)

    def adjacent = List(u, d, l, r, ur, ul, dr, dl)
  }

  def showSeats(seats: Seats): String = {
    val (Coord(minX, minY), Coord(maxX, maxY)) = extent(seats)
    def showRow(y: Int): String =
      (minX to maxX).foldLeft(new StringBuilder()) { case (sb, x) =>
        seats.get(Coord(x, y)) match {
          case Some(Empty) => sb.append("L")
          case Some(Taken) => sb.append("#")
          case None        => sb.append(".")
        }
      }.toString

    (for {
      y <- minY to maxY
      r =  showRow(y)
    } yield r).mkString("\n")
  }

  def extent(seats: Seats): (Coord, Coord) = {
    val coords = seats.keySet
    (Coord(coords.minBy(_.x).x, coords.minBy(_.y).y), Coord(coords.maxBy(_.x).x, coords.maxBy(_.y).y))
  }


  def parseLine(y: Int, s: String): List[(Coord, Seat)] =
    LazyList.from(0)
      .zip(s)
      .flatMap { case (x, c) =>
        c match {
          case 'L' => Some((Coord(x, y), Empty))
          case '#' => Some((Coord(x, y), Taken))
          case _   => None
        }
      }.toList

  def parseInput(input: Iterator[String]): Seats =
    LazyList.from(0)
      .zip(input)
      .flatMap { case (y, s) => parseLine(y, s) }
      .toMap

  def readFile(f: String): Seats =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day11.txt"

  def testData = """L.LL.LL.LL
                   |LLLLLLL.LL
                   |L.L.L..L..
                   |LLLL.LL.LL
                   |L.LL.LL.LL
                   |L.LLLLL.LL
                   |..L.L.....
                   |LLLLLLLLLL
                   |L.LLLLLL.L
                   |L.LLLLL.LL""".stripMargin.linesIterator

  def testData2 = """.......#.
                    |...#.....
                    |.#.......
                    |.........
                    |..#L....#
                    |....#....
                    |.........
                    |#........
                    |...#.....""".stripMargin.linesIterator

  def testData3 = """.##.##.
                    |#.#.#.#
                    |##...##
                    |...L...
                    |##...##
                    |#.#.#.#
                    |.##.##.""".stripMargin.linesIterator
}
