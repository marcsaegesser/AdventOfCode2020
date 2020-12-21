package advent

object Day20 {

  def part1a(tiles: List[Tile]): Long = {
    val freqs = tiles.flatMap(_.sides).groupBy(identity).map { case (k, v) => (k, v.size)}

    tiles
      .map(t => (t.id, t.sides.filter(s => freqs(s) == 1).size))
      .filter(_._2 == 4)
      .map(_._1.toLong)
      .product
  }

  def part1(tiles: List[Tile]) = {
    val allAdj = tiles.flatMap(t => t.sides.map((t.id, _))).groupBy(_._2).map { case (k, v) => (k, v.map(_._1))}
    val adj = tiles.map(t => (t.id, t.sides.map(allAdj(_)).flatMap(_.filterNot(_ == t.id)).toSet))

    adj.filter(_._2.size == 2).map(_._1.toLong).product
  }

  @annotation.nowarn
  def buildBoard(tiles: List[Tile]) = {
    val tileMap = tiles.map(t => (t.id, t)).toMap
    val allAdj = tiles.flatMap(t => t.sides.map((t.id, _))).groupBy(_._2).map { case (k, v) => (k, v.map(_._1))}

    val structure: Structure =
      tiles
        .map(t =>
          (t.id, t.sides.map(s => (s, allAdj(s)))
            .filter(_._2.size == 2)
            .map { case (s, ids) => (s, ids.filterNot(_ == t.id).head) })
        ).map { case (id, ss) =>
            (id, ss.map { case (s, i) =>
              (s, i, tileMap(id).findSide(s), tileMap(i).findSide(s))
            }.groupBy(_._2).map { case (k, v) => (k, v.map(vp => (vp._3, vp._4))) }.toList
            )}
        .toMap

    ???
  }

  type Board = Map[Coord, Tile]
  type Rule = (Int, List[(Option[Side], Option[Side])])
  type Structure = Map[Int, List[Rule]]

  // @annotation.nowarn
  def processRules(board: Board, tile: Tile, coord: Coord, remaining: Map[Int, Tile], structure: Structure): (Board, Map[Int, Tile]) = {
    println(s"processRules:  $tile, $coord")
    val nextBoard = board.updated(coord, tile)
    val nextRemaining = remaining - tile.id

    val rules = structure(tile.id)
    rules.foldLeft((nextBoard, nextRemaining)) { case ((b, r), (n, ll)) =>
      println(s"$n - $ll")
      if(r.contains(n)) {
        val (d, nextTile) =
          ll match {
            case (Some(a), Some(b)) :: (None, None) :: Nil => (a, rotateTile(r(n), b, a.opposite))
            case (Some(a), None) :: (None, Some(b)) :: Nil => (a, a.flipF(rotateTile(r(n), b, a.opposite)))
            case _                                         => throw new Exception("invalid structure")
          }
        val nextCoord =
          d match {
            case Top    => coord.up
            case Right  => coord.right
            case Bottom => coord.down
            case Left   => coord.left
          }
        processRules(b, nextTile, nextCoord, r, structure)
      } else {
        (b, r)
      }
    }
  }

  def processRulesX(board: Board, tile: Tile, coord: Coord, remaining: Map[Int, Tile], structure: Structure): (Board, Map[Int, Tile]) = {
    println(s"processRules:  $tile, $coord")
    if(!remaining.contains(tile.id)) (board, remaining)
    else {
      val rules = structure(tile.id)
      rules.foldLeft((board, remaining)) { case (b, (n, ll)) =>
        println(s"$n - $ll")
        val (d, nextTile) =
          ll match {
            case (Some(a), Some(b)) :: (None, None) :: Nil => (a, rotateTile(remaining(n), b, a.opposite))
            case (Some(a), None) :: (None, Some(b)) :: Nil => (a, a.flipF(rotateTile(remaining(n), b, a.opposite)))
            case _                                         => throw new Exception("invalid structure")
          }
        val nextCoord =
          d match {
            case Top    => coord.up
            case Right  => coord.right
            case Bottom => coord.down
            case Left   => coord.left
          }
        processRules(board.updated(coord, tile), nextTile, nextCoord, remaining - tile.id, structure)
      }
    }
  }

  def fubar(tiles: List[Tile]) = {
    val tileMap = tiles.map(t => (t.id, t)).toMap
    val allAdj = tiles.flatMap(t => t.sides.map((t.id, _))).groupBy(_._2).map { case (k, v) => (k, v.map(_._1))}
    // val adj = tiles.map(t => (t.id, t.sides.map(allAdj(_)).flatMap(_.filterNot(_ == t.id)).toSet))

    val _ =
      tiles
        .map(t =>
          (t.id, t.sides.map(s => (s, allAdj(s)))
            .filter(_._2.size == 2)
            .map { case (s, ids) => (s, ids.filterNot(_ == t.id).head) })
        ).map { case (id, ss) =>
            (id, ss.map { case (s, i) =>
              (s, i, tileMap(id).findSide(s), tileMap(i).findSide(s))
            })}

    val _ =
      tiles
        .map(t =>
          (t.id, t.sides.map(s => (s, allAdj(s)))
            .filter(_._2.size == 2)
            .map { case (s, ids) => (s, ids.filterNot(_ == t.id).head) })
        ).map { case (id, ss) =>
            (id, ss.map { case (s, i) =>
              (s, i, tileMap(id).findSide(s), tileMap(i).findSide(s))
            }.groupBy(_._2).map { case (k, v) => (k, v.map(vp => (vp._3, vp._4))) }
            )}

    val x =
      tiles
        .map(t =>
          (t.id, t.sides.map(s => (s, allAdj(s)))
            .filter(_._2.size == 2)
            .map { case (s, ids) => (s, ids.filterNot(_ == t.id).head) })
        ).map { case (id, ss) =>
            (id, ss.map { case (s, i) =>
              (s, i, tileMap(id).findSide(s), tileMap(i).findSide(s))
            }.groupBy(_._2).map { case (k, v) => (k, v.map(vp => (vp._3, vp._4))) }.toList
            )}
        .toMap

    //res0.map { case (id, ss) => (id, ss.map { case (s, i) => (s, i, tileMap(id).findSide(s), tileMap(i).findSide(s))})}

    x
  }

  case class Coord(r: Int, c: Int) {
    def up    = Coord(r-1,   c)
    def down  = Coord(r+1, c)
    def right = Coord(r,   c+1)
    def left  = Coord(r,   c-1)
  }

  sealed trait Side { def opposite: Side; def value: Int; def flipF: (Tile) => Tile }
  case object Top    extends Side { def opposite = Bottom; val value = 0; val flipF = flipH }
  case object Right  extends Side { def opposite = Left ;  val value = 1; val flipF = flipV }
  case object Bottom extends Side { def opposite = Top;    val value = 2; val flipF = flipH }
  case object Left   extends Side { def opposite = Right;  val value = 3; val flipF = flipV  }

  def rotateN(tile: Tile, n: Int): Tile =
    if(n == 0) tile
    else rotateN(tile, n-1)

  def numRotations(start: Side, finish: Side): Int = {
    (finish.value - start.value + 4) % 4
  }

  case class Tile(id: Int, data: Vector[Vector[Char]]) {
    val top = data.head.mkString
    val bottom = data.last.mkString
    val left = data.map(_.head).mkString
    val right = data.map(_.last).mkString

    val sides: List[String] = List(top, right, bottom, left).flatMap(s => List(s, s.reverse))

    def findSide(s: String): Option[Side] = {
      if(s == top)         Some(Top)
      else if(s == bottom) Some(Bottom)
      else if(s == right)  Some(Right)
      else if(s == left)   Some(Left)
      else                 None
    }

    def rotate: Tile = Tile(id, data.transpose.map(_.reverse))
    def flipH: Tile = Tile(id, data.map(_.reverse))
    def flipV: Tile = Tile(id, data.reverse)

    override def toString: String = s"Tile $id\n${show}"
    def show: String = data.map(_.mkString).mkString("\n")
  }

  def rotateTile(tile: Tile, from: Side, to: Side): Tile = {
    rotateN(tile, numRotations(from, to))
  }

  def flipH(tile: Tile): Tile = tile.flipH
  def flipV(tile: Tile): Tile = tile.flipV


  // def tileVariations(tile: Tile): Set[Tile] = {
  //   val r1 = rotate(tile.data)
  //   val r2 = rotate(r1)
  //   val r3 = rotate(r2)
  //   val rs = Set(tile.data, r1, r2, r3)
  //   rs
  //     .foldLeft(rs) { case (accum, r) => accum + flipH(r) + flipV(r) }
  //     .map(Tile(tile.id, _))
  // }

  // def flipH(data: Vector[Vector[Char]]): Vector[Vector[Char]] = data.map(_.reverse)
  // def flipV(data: Vector[Vector[Char]]): Vector[Vector[Char]] = data.reverse
  // def rotate(data: Vector[Vector[Char]]): Vector[Vector[Char]] = flipH(data.transpose)



  val idRegex = """Tile\s+(\d+):""".r

  @annotation.nowarn
  def parseTile(input: List[String]): Tile = {
    val idRegex(id) = input.head
    val data = input.tail.map(_.toVector).toVector
    Tile(id.toInt, data)
  }

  def parseInput(input: Iterator[String]): List[Tile] = {
    def helper(ts: List[Tile], is: Iterator[String]): List[Tile] = {
      if(is.isEmpty) ts
      else {
        val t = parseTile(is.takeWhile(!_.isEmpty).toList)
        helper(t :: ts, is)
      }
    }

    helper(List.empty[Tile], input)
  }

  def readFile(f: String): List[Tile] =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day20.txt"

  val testData = """Tile 2311:
                   |..##.#..#.
                   |##..#.....
                   |#...##..#.
                   |####.#...#
                   |##.##.###.
                   |##...#.###
                   |.#.#.#..##
                   |..#....#..
                   |###...#.#.
                   |..###..###
                   |
                   |Tile 1951:
                   |#.##...##.
                   |#.####...#
                   |.....#..##
                   |#...######
                   |.##.#....#
                   |.###.#####
                   |###.##.##.
                   |.###....#.
                   |..#.#..#.#
                   |#...##.#..
                   |
                   |Tile 1171:
                   |####...##.
                   |#..##.#..#
                   |##.#..#.#.
                   |.###.####.
                   |..###.####
                   |.##....##.
                   |.#...####.
                   |#.##.####.
                   |####..#...
                   |.....##...
                   |
                   |Tile 1427:
                   |###.##.#..
                   |.#..#.##..
                   |.#.##.#..#
                   |#.#.#.##.#
                   |....#...##
                   |...##..##.
                   |...#.#####
                   |.#.####.#.
                   |..#..###.#
                   |..##.#..#.
                   |
                   |Tile 1489:
                   |##.#.#....
                   |..##...#..
                   |.##..##...
                   |..#...#...
                   |#####...#.
                   |#..#.#.#.#
                   |...#.#.#..
                   |##.#...##.
                   |..##.##.##
                   |###.##.#..
                   |
                   |Tile 2473:
                   |#....####.
                   |#..#.##...
                   |#.##..#...
                   |######.#.#
                   |.#...#.#.#
                   |.#########
                   |.###.#..#.
                   |########.#
                   |##...##.#.
                   |..###.#.#.
                   |
                   |Tile 2971:
                   |..#.#....#
                   |#...###...
                   |#.#.###...
                   |##.##..#..
                   |.#####..##
                   |.#..####.#
                   |#..#.#..#.
                   |..####.###
                   |..#.#.###.
                   |...#.#.#.#
                   |
                   |Tile 2729:
                   |...#.#.#.#
                   |####.#....
                   |..#.#.....
                   |....#..#.#
                   |.##..##.#.
                   |.#.####...
                   |####.#.#..
                   |##.####...
                   |##..#.##..
                   |#.##...##.
                   |
                   |Tile 3079:
                   |#.#.#####.
                   |.#..######
                   |..#.......
                   |######....
                   |####.#..#.
                   |.#...#.##.
                   |#.#####.##
                   |..#.###...
                   |..#.......
                   |..#.###...
                   |""".stripMargin.linesIterator
}
