package advent

object Day24 {

  def run(): Unit = {
    val paths = readFile(inputFile)
    println(s"Day24.part1 = ${part1(paths)}")
    println(s"Day24.part2 = ${part2(paths)}")
  }

  def part1(paths: List[HexCoord]): Int =
    paths
      .groupBy(identity)
      .values
      .filter { _.size % 2 == 1 }
      .size

  def part2(paths: List[HexCoord]): Int = {
    val grid =
      paths
        .groupBy(identity)
        .filter { case (_, v) =>  v.size % 2 == 1 }
        .keySet

    runN(100, grid).size
  }

  def step(grid: Grid): Grid = {
    reachable(grid).filter(applyRules(_, grid))
  }

  def runN(n: Int, grid: Grid): Grid =
    if(n == 0) grid
    else       runN(n-1, step(grid))

  def reachable(grid: Grid): Set[HexCoord] =
    grid.foldLeft(Set.empty[HexCoord]) { case (a, c) => a ++ c.adjacent }

  def applyRules(c: HexCoord, grid: Grid): Boolean = {
    (grid(c), (c.adjacent & grid).size) match {
      case (true, sz) if sz == 0 || sz > 2 => false
      case (true, _)                       => true
      case (false, sz) if sz == 2          => true
      case (false, _)                      => false
    }
  }

  type Grid = Set[HexCoord]

  // A skew-y hexagonal coordinate system
  case class HexCoord(x: Int, y: Int) {
    def e  = HexCoord(x+1, y)
    def se = HexCoord(x+1, y-1)
    def sw = HexCoord(x,   y-1)
    def w  = HexCoord(x-1, y)
    def nw = HexCoord(x-1, y+1)
    def ne = HexCoord(x,   y+1)

    def adjacent: Set[HexCoord] = Set(this.e, this.se, this.sw, this.w, this.nw, this.ne)
  }

  def parsePathCoord(path: String): HexCoord ={
    def helper(coord: HexCoord, ds: List[Char]): HexCoord =
      ds match {
        case Nil             => coord
        case 'n' :: 'w' :: t => helper(coord.nw, t)
        case 'n' :: 'e' :: t => helper(coord.ne, t)
        case 's' :: 'w' :: t => helper(coord.sw, t)
        case 's' :: 'e' :: t => helper(coord.se, t)
        case 'e' :: t        => helper(coord.e, t)
        case 'w' :: t        => helper(coord.w, t)
        case _               => throw new Exception("Invalid input")
      }

    helper(HexCoord(0, 0), path.toList)
  }

  def parseInput(input: Iterator[String]): List[HexCoord] =
    input.map(parsePathCoord).toList

  def readFile(f: String): List[HexCoord] =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day24.txt"

  def testData = """sesenwnenenewseeswwswswwnenewsewsw
                   |neeenesenwnwwswnenewnwwsewnenwseswesw
                   |seswneswswsenwwnwse
                   |nwnwneseeswswnenewneswwnewseswneseene
                   |swweswneswnenwsewnwneneseenw
                   |eesenwseswswnenwswnwnwsewwnwsene
                   |sewnenenenesenwsewnenwwwse
                   |wenwwweseeeweswwwnwwe
                   |wsweesenenewnwwnwsenewsenwwsesesenwne
                   |neeswseenwwswnwswswnw
                   |nenwswwsewswnenenewsenwsenwnesesenew
                   |enewnwewneswsewnwswenweswnenwsenwsw
                   |sweneswneswneneenwnewenewwneswswnese
                   |swwesenesewenwneswnwwneseswwne
                   |enesenwswwswneneswsenwnewswseenwsese
                   |wnwnesenesenenwwnenwsewesewsesesew
                   |nenewswnwewswnenesenwnesewesw
                   |eneswnwswnwsenenwnwnwwseeswneewsenese
                   |neswnwewnwnwseenwseesewsenwsweewe
                   |wseweeenwnesenwwwswnew""".stripMargin.linesIterator
}

object Day24a {

  def part1(paths: List[Path]): Int =
    paths
      .map(minimumPath)
      .groupBy(identity)
      .values
      .map(_.size)
      .filter(_ % 2 == 1)
      .size

  def step(grid: Grid): Grid = {
    reachable(grid).filter(applyRules(_, grid))
  }

  def runN(n: Int, grid: Grid): Grid =
    if(n == 0) grid
    else       runN(n-1, step(grid))

  def reachable(grid: Grid): Set[HexCoord] =
    grid.foldLeft(Set.empty[HexCoord]) { case (a, c) => a ++ c.adjacent }

  def applyRules(c: HexCoord, grid: Grid): Boolean = {
    (grid(c), (c.adjacent & grid).size) match {
      case (true, sz) if sz == 0 || sz > 2 => false
      case (true, _)                       => true
      case (false, sz) if sz == 2          => true
      case (false, _)                      => false
    }
  }

  type Grid = Set[HexCoord]

  // A skew-y hexagonal coordinate system
  case class HexCoord(x: Int, y: Int) {
    def e  = HexCoord(x+1, y)
    def se = HexCoord(x+1, y-1)
    def sw = HexCoord(x,   y-1)
    def w  = HexCoord(x-1, y)
    def nw = HexCoord(x-1, y+1)
    def ne = HexCoord(x,   y+1)

    def adjacent: Set[HexCoord] = Set(this.e, this.se, this.sw, this.w, this.nw, this.ne)
  }

  type Dir = Int
  val E  = 0
  val SE = 1
  val SW = 2
  val W  = 3
  val NW = 4
  val NE = 5

  // type Path = Map[Dir, Int]  // The number of steps in each direction
  case class Path(steps: Map[Dir, Int]) {
    def clean: Path = Path(steps.filter { case (k, v) => v != 0 })
    def stepCount: Int = steps.values.sum
    def stepCountDir(dir: Dir): Int = steps.getOrElse(dir, 0)
  }

  // Combine two directions, wrapping around the hexagon
  def dirPlus(a: Dir, b: Int): Dir = (a + b) % 6

  /** Cancel steps on opposite hexagonal directions.
    *
    * For example, if the state contains 3 N steps and 2 S steps
    * the result should be a single N step.
    */
  def cancelOpposite(dir: Dir)(state: Path): Path = {
    val dir2 = dirPlus(dir, 3)
    val changes =
      (state.stepCountDir(dir), state.stepCountDir(dir2)) match {
        case (_, 0)           => List()
        case (0, _)           => List()
        case (a, b) if a >= b => List((dir -> (a-b)), (dir2 -> 0))
        case (a, b)           => List((dir -> 0),     (dir2 -> (b-a)))

      }
    Path(state.steps ++ changes)
  }

  /** Cancel steps 'adjacent' steps.
    *
    * For example one N step and one SE step reduce to a single NE
    * step.
    */
  def reduceAdjacent(dir: Dir)(state: Path): Path = {
    val dir2 = dirPlus(dir, 2)
    val dir3 = dirPlus(dir, 1)
    val changes =
      (state.stepCountDir(dir), state.stepCountDir(dir2), state.stepCountDir(dir3)) match {
        case (a, 0, _)           => List()
        case (0, b, _)           => List()
        case (a, b, c) if a >= b => List((dir -> (a-b)), (dir2 -> 0)    , (dir3 -> (c+b)))
        case (a, b, c)           => List((dir -> 0)    , (dir2 -> (b-a)), (dir3 -> (c+a)))
      }
    Path(state.steps ++ changes)
  }

  /** Apply all reductions to the given PathState
    */
  val reduceAll: Path => Path =
    (
      (E to NE).map(d => cancelOpposite(d)(_)) ++   // Cancel all opposite directions
        (E to NE).map(d => reduceAdjacent(d)(_))    // Cancel all adjacent directions
    ).reduce(_ compose _)                           // Compose everything into a single function

  def minimumPath(path: Path): Path = {
    val p = reduceAll(path)
    if(p.stepCount == path.stepCount) path.clean
    else                              minimumPath(p)
  }


  def parsePath(path: String): Path ={
    def helper(accum: List[Int], ds: List[Char]): List[Int] =
      ds match {
        case Nil             => accum.reverse
        case 'n' :: 'w' :: t => helper(NW :: accum, t)
        case 'n' :: 'e' :: t => helper(NE :: accum, t)
        case 's' :: 'w' :: t => helper(SW :: accum, t)
        case 's' :: 'e' :: t => helper(SE :: accum, t)
        case 'e' :: t        => helper(E :: accum, t)
        case 'w' :: t        => helper(W :: accum, t)
        case _               => throw new Exception("Invalid input")
      }

    val steps = helper(Nil, path.toList).groupBy(identity).map { case (d, l) => (d, l.size) }
    Path(steps)
  }

  def parsePathCoord(path: String): HexCoord ={
    def helper(coord: HexCoord, ds: List[Char]): HexCoord =
      ds match {
        case Nil             => coord
        case 'n' :: 'w' :: t => helper(coord.nw, t)
        case 'n' :: 'e' :: t => helper(coord.ne, t)
        case 's' :: 'w' :: t => helper(coord.sw, t)
        case 's' :: 'e' :: t => helper(coord.se, t)
        case 'e' :: t        => helper(coord.e, t)
        case 'w' :: t        => helper(coord.w, t)
        case _               => throw new Exception("Invalid input")
      }

    helper(HexCoord(0, 0), path.toList)
  }

  def parseInput(input: Iterator[String]): List[Path] =
    input.map(parsePath).toList

  def parseInputCoord(input: Iterator[String]): List[HexCoord] =
    input.map(parsePathCoord).toList

  def readFile(f: String): List[Path] =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day24.txt"

  def testData = """sesenwnenenewseeswwswswwnenewsewsw
                   |neeenesenwnwwswnenewnwwsewnenwseswesw
                   |seswneswswsenwwnwse
                   |nwnwneseeswswnenewneswwnewseswneseene
                   |swweswneswnenwsewnwneneseenw
                   |eesenwseswswnenwswnwnwsewwnwsene
                   |sewnenenenesenwsewnenwwwse
                   |wenwwweseeeweswwwnwwe
                   |wsweesenenewnwwnwsenewsenwwsesesenwne
                   |neeswseenwwswnwswswnw
                   |nenwswwsewswnenenewsenwsenwnesesenew
                   |enewnwewneswsewnwswenweswnenwsenwsw
                   |sweneswneswneneenwnewenewwneswswnese
                   |swwesenesewenwneswnwwneseswwne
                   |enesenwswwswneneswsenwnewswseenwsese
                   |wnwnesenesenenwwnenwsewesewsesesew
                   |nenewswnwewswnenesenwnesewesw
                   |eneswnwswnwsenenwnwnwwseeswneewsenese
                   |neswnwewnwnwseenwseesewsenwsweewe
                   |wseweeenwnesenwwwswnew""".stripMargin.linesIterator
}
