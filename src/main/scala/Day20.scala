package advent

/**
  * This is REALLY, REALLY, rough and ugly, but it does work on my puzzle input.
  */
object Day20 {

  def run(): Unit = {
    val tiles = readFile(inputFile)
    println(s"Day20.part1 = ${part1(tiles)}")
    println(s"Day20.part2 = ${part2(tiles)}")
  }

  def part1(tiles: List[Tile]) = {
    val allAdj = tiles.flatMap(t => t.sides.map((t.id, _))).groupBy(_._2).map { case (k, v) => (k, v.map(_._1))}
    val adj = tiles.map(t => (t.id, t.sides.map(allAdj(_)).flatMap(_.filterNot(_ == t.id)).toSet))

    adj.filter(_._2.size == 2).map(_._1.toLong).product
  }

  def part2(tiles: List[Tile]) = {
    val image = mkImage(buildBoard(tiles))
    val pattern = parsePattern(monster)
    val found = findMonsters(image, pattern).filterNot(_.isEmpty).head

    (image -- found).size
  }

  def buildBoard(tiles: List[Tile]) = {
    val tileMap = tiles.map(t => (t.id, t)).toMap
    val allAdj = tiles.flatMap(t => t.sides.map((t.id, _))).groupBy(_._2).map { case (k, v) => (k, v.map(_._1))}
    val structure = tiles.map(t => (t.id, t.sides.map(s => (s, allAdj(s))).filter(_._2.size == 2).map { case (s, ids) => (s, ids.filterNot(_ == t.id).head) }.grouped(2).map(_.head).toList)).toMap

    arrange(Map.empty[Coord, Tile], tiles.head, Coord(0, 0), tileMap, structure)._1
  }

  def mkImage(board: Board): Image = {
    val (Coord(minR, minC), _) = extent(board)
    val tileSize = board.head._2.data.head.size - 2

    board
      .map { case (c, t) => (c, t.core) } // Replace the tiles with their cores
      .foldLeft(Set.empty[Coord]) { case (i, (Coord(r, c), t)) =>
        val tlc = Coord((r - minR)*tileSize, (c - minC)*tileSize)
        t.data
          .zipWithIndex
          .foldLeft(Set.empty[Coord]) { case (a, (v, dr)) =>
            v.zipWithIndex.collect { case ('#', dc) => Coord(tlc.r + dr, tlc.c + dc) }.toSet ++ a
          } ++ i
      }
  }

  def findMonsters(img: Image, pattern: Vector[Vector[Char]]) = {
    val (Coord(minR, minC), Coord(maxR, maxC)) = extent(img)
    val allPats = allPatterns(pattern).map(mkPatternSet)

    allPats.toList.map { pat =>
      (minR to maxR).foldLeft(Set.empty[Coord]) { case (a, r) =>
        (minC to maxC).foldLeft(a) { case (aa, c) =>
          aa ++ checkPattern(img, pat, Coord(r, c))
        } ++ a
      }
    }
  }

  def allPatterns(pattern: Vector[Vector[Char]]): Set[Vector[Vector[Char]]] = {
    def rotate(data: Vector[Vector[Char]]) = data.transpose.map(_.reverse)
    def flipH(data: Vector[Vector[Char]])  = data.map(_.reverse)
    def flipV(data: Vector[Vector[Char]])  = data.reverse

    val rots = Set(pattern, rotate(pattern), rotate(rotate(pattern)), rotate(rotate(rotate(pattern))))
    val fvs = rots.map(flipV)
    val fhs = rots.map(flipH)

    rots ++ fvs ++ fhs
  }

  def mkPatternSet(patternText: Vector[Vector[Char]]): Pattern = {
    patternText
      .zipWithIndex
      .foldLeft(Set.empty[Coord]) { case (a, (v, r)) =>
        v.zipWithIndex.collect { case ('#', c) => Coord(r, c) }.toSet ++ a
      }
  }

  def checkPattern(img: Image, pattern: Pattern, pos: Coord): Set[Coord] = {
    val s = pattern.map(_.offset(pos))
    if((s & img).size == pattern.size) s
    else                               Set()
  }

  type TileMap = Map[Int, Tile]
  type Board = Map[Coord, Tile]
  type Adjacent = (String, Int)
  type Structure = Map[Int, List[Adjacent]]
  type Image = Set[Coord]
  type Pattern = Set[Coord]

  def arrange(board: Board, tile: Tile, coord: Coord, remaining: TileMap, structure: Structure): (Board, TileMap) = {
    // Place the current tile on the board
    val nextBoard = board.updated(coord, tile)
    val nextRemaining = remaining - tile.id

    // Place adjancent tiles
    structure(tile.id).foldLeft((nextBoard, nextRemaining)) { case ((b, r), (s, t)) =>
      if(!r.contains(t)) (b, r)  // Only if it hasn't already been placed
      else {
        val joinSide = tile.findSide(s).get       // The side of this tile we're joining to
        val otherJoinSide = r(t).findSide(s).get  // Where the target side is currently on the other tile
        val jt = rotateTile(r(t), otherJoinSide, joinSide.opposite)  // Rotate the other tile to the correct orientation
        val joinTile =                                               // Flip the other tile if necessary
          if(tile.side(joinSide) == jt.side(joinSide.opposite)) jt
          else                                                  joinSide.flipF(jt)
        val nextCoord =                           // Pick the next coord based on the join side
          joinSide match {
            case Top    => coord.up
            case Right  => coord.right
            case Bottom => coord.down
            case Left   => coord.left
          }

        arrange(b, joinTile, nextCoord, r, structure)  // Recurse to place this tile and its adjacents
      }
    }

  }

  def extent(board: Board): (Coord, Coord) = {
    (Coord(board.map(_._1.r).min, board.map(_._1.c).min),
    Coord(board.map(_._1.r).max, board.map(_._1.c).max))
  }

  def extent(img: Image): (Coord, Coord) = {
    (Coord(img.minBy(_.r).r, img.minBy(_.c).c), Coord(img.maxBy(_.r).r, img.maxBy(_.c).c))
  }

  case class Coord(r: Int, c: Int) {
    def up    = Coord(r-1,   c)
    def down  = Coord(r+1, c)
    def right = Coord(r,   c+1)
    def left  = Coord(r,   c-1)

    def offset(other: Coord): Coord =
      Coord(r + other.r, c + other.c)
  }

  sealed trait Side { def opposite: Side; def value: Int; def flipF: (Tile) => Tile }
  case object Top    extends Side { def opposite = Bottom; val value = 0; val flipF = flipH }
  case object Right  extends Side { def opposite = Left ;  val value = 1; val flipF = flipV }
  case object Bottom extends Side { def opposite = Top;    val value = 2; val flipF = flipH }
  case object Left   extends Side { def opposite = Right;  val value = 3; val flipF = flipV  }

  def rotateN(tile: Tile, n: Int): Tile =
    if(n == 0) tile
    else       rotateN(tile.rotate, n-1)

  def numRotations(start: Side, finish: Side): Int = {
    (finish.value - start.value + 4) % 4
  }

  case class Tile(id: Int, data: Vector[Vector[Char]]) {
    val top    = data.head.mkString
    val bottom = data.last.mkString
    val left   = data.map(_.head).mkString
    val right  = data.map(_.last).mkString

    def side(s: Side): String =
      s match {
        case Top    => top
        case Right  => right
        case Bottom => bottom
        case Left   => left
      }

    val sides: List[String] = List(top, right, bottom, left).flatMap(s => List(s, s.reverse))

    def findSide(s: String): Option[Side] = {
      if(s == top)                 Some(Top)
      else if(s == top.reverse)    Some(Top)
      else if(s == bottom)         Some(Bottom)
      else if(s == bottom.reverse) Some(Bottom)
      else if(s == right)          Some(Right)
      else if(s == right.reverse)  Some(Right)
      else if(s == left)           Some(Left)
      else if(s == left.reverse)   Some(Left)
      else                         None
    }

    def core: Tile =
      Tile(id, data.drop(1).init.map(_.tail.init))

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

  // val monster = Set(Coord(0, 18),
  //   Coord(1, 0), Coord(1, 5), Coord(1, 6), Coord(1, 11), Coord(1, 12), Coord(1, 17), Coord(1, 18), Coord(1, 19),
  //   Coord(2, 1), Coord(2, 4), Coord(2, 7), Coord(2, 10, Coord(2, 13), Coord(2, 16))
  // )

  def parsePattern(lines: Iterator[String]): Vector[Vector[Char]] =
    lines.map(_.toVector).toVector

  def monster = """                  # 
                  |#    ##    ##    ###
                  | #  #  #  #  #  #   """.stripMargin.linesIterator

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

  val test = """.#.#..#.##...#.##..#####
               |###....#.#....#..#......
               |##.##.###.#.#..######...
               |###.#####...#.#####.#..#
               |##.#....#.##.####...#.##
               |...########.#....#####.#
               |....#..#...##..#.#.###..
               |.####...#..#.....#......
               |#..#.##..#..###.#.##....
               |#.####..#.####.#.#.###..
               |###.#.#...#.######.#..##
               |#.####....##..########.#
               |##..##.#...#...#.#.#.#..
               |...#..#..#.#.##..###.###
               |.#.#....#.##.#...###.##.
               |###.#...#..#.##.######..
               |.#.#.###.##.##.#..#.##..
               |.####.###.#...###.#..#.#
               |..#.#..#..#.#.#.####.###
               |#..####...#.#.#.###.###.
               |#####..#####...###....##
               |#.##..#..#...#..####...#
               |.#.###..##..##..####.##.
               |...###...##...#...#..###""".stripMargin
}
