package advent

object Day25 {

  def run(): Unit = {
    val keys = readFile(inputFile)
    println(s"Day25.part1 = ${part1(keys)}")
  }

  def part1(keys: (Long, Long)): Long = {
    val (a, b) = keys
    val (an, bn) = findLoopSizes(a, b)

    transform(a, bn)
  }

  def findLoopSizes(a: Long, b: Long): (Long, Long) = {
    def helper(accum: (Option[Long], Option[Long]), v: Long, n: Long): (Long, Long) = {
      val nextAccum =
        if(v == a)      (Some(n), accum._2)
        else if(v == b) (accum._1, Some(n))
        else            accum

      nextAccum match {
        case (Some(na), Some(nb)) => (na, nb)
        case _                    => helper(nextAccum, (v * 7L) % div, n+1)
      }
    }

    helper((None, None), 1, 0)
  }

  def transform(subj: Long, loopSize: Long): Long = {
    def helper(n: Long, v: Long): Long =
      if(n == 0) v
      else       helper(n-1, (v * subj) % div)

    helper(loopSize, 1)
  }


  def transformStep(v: Long, subj: Long) =
    (v * subj) % div

  val div = 20201227L

  def parseInput(lines: Iterator[String]): (Long, Long) =
    lines.toList.map(_.toLong) match {
      case a :: b :: Nil => (a, b)
      case _             => throw new Exception("Invalid input")
    }

  def readFile(f: String): (Long, Long) =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day25.txt"

  def testData = """5764801
                   |17807724""".stripMargin.linesIterator
}
