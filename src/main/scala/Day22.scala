package advent

object Day22 {

  def run(): Unit = {
    val game = readFile(inputFile)
    println(s"Day 22.part1 = ${part1(game)}")
    println(s"Day 22.part2 = ${part2(game)}")
  }

  def part1(game: Game): Int = {
    score(playGame(game))
  }

  def part2(game: Game): Int = {
    playGame2(game, (Set(), Set())) match {
      case Game(Nil, Nil) => 0
      case Game(p1, Nil)  => score(p1)
      case Game(Nil, p2)  => score(p2)
      case Game(_, _)     => 0
    }
  }

  def score(deck: List[Int]): Int =
    deck
      .reverse
      .zip(LazyList.from(1))
      .map { case (a, b) => a*b }
      .sum


  def playGame(game: Game): List[Int] = {
    if(game.p1.isEmpty)      game.p2
    else if(game.p2.isEmpty) game.p1
    else {
      (game.p1, game.p2) match {
        case (a :: as, b :: bs) if a > b => playGame(Game(as ++ List(a, b), bs))
        case (a :: as, b :: bs)          => playGame(Game(as, bs ++ (List(b, a))))
        case _                           => throw new Exception("Invalid input")
      }
    }
  }

  def playGame2(game: Game, history: (Set[List[Int]], Set[List[Int]])): Game = {
    if(game.p1.isEmpty || game.p2.isEmpty) game
    else
      game match { case Game(p1, p2) =>
        if(history._1.contains(p1) || history._2.contains(p2)) Game(p1, Nil)
        else
          (p1, p2) match {
            case (a :: as, b :: bs) if a > as.size || b > bs.size =>
              if(a < b) playGame2(Game(as, bs ++ List(b, a)), (history._1 + p1, history._2 + p2))
              else      playGame2(Game(as ++ List(a, b), bs), (history._1 + p1, history._2 + p2))
            case (a :: as, b :: bs) =>
              playGame2(Game(as.take(a), bs.take(b)), (Set(), Set())) match {
                case Game(Nil, Nil) => throw new Exception("Everybody lost!")
                case Game(pp1, Nil) => playGame2(Game(as ++ List(a, b), bs), (history._1 + p1, history._2 + p2))
                case Game(Nil, pp2) => playGame2(Game(as, bs ++ List(b, a)), (history._1 + p1, history._2 + p2))
                case Game(_, _)     => throw new Exception("Bad game")
              }
            case _ => throw new Exception("Invalid game")
          }
      }
  }

  case class Game(p1: List[Int], p2: List[Int])

  def parsePlayer(lines: List[String]): List[Int] =
    lines.drop(1).map(_.toInt)

  def parseInput(lines: Iterator[String]) = {
    Game(parsePlayer(lines.takeWhile(!_.isEmpty).toList), parsePlayer(lines.takeWhile(!_.isEmpty).toList))
  }

  def readFile(f: String): Game =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day22.txt"

  val testData = """Player 1:
                   |9
                   |2
                   |6
                   |3
                   |1
                   |
                   |Player 2:
                   |5
                   |8
                   |4
                   |7
                   |10""".stripMargin.linesIterator
}
