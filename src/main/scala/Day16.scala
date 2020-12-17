package advent

object Day16 {

  def run(): Unit = {
    val data = readFile(inputFile)
    println(s"Day16.part1 = ${part1(data)}")
    println(s"Day16.part2 = ${part2(data)}")
  }

  def part1(data: TicketData): Int = {
    data.nearby.flatten.filterNot(n => checkRules(data.rules, n)).sum
  }

  def part2(data: TicketData) = {
    val tickets = data.nearby.filter(isValidTicket(data.rules, _))
    val colNames = determineColumnNames(data.rules, tickets)
    data.ticket
      .zipWithIndex
      .map { case (v, i) => (colNames(i), v) }
      .filter(_._1.startsWith("departure"))
      .map(_._2.toLong)
      .product
  }


  def checkRules(rules: Map[String, List[(Int, Int)]], n: Int): Boolean =
    rules.values.exists(_.exists { case (min, max) => n >= min && n <= max })

  def checkRule(rule: List[(Int, Int)], n: Int): Boolean =
    rule.exists { case (min, max) => n >= min && n <= max }

  def isValidTicket(rules: Map[String, List[(Int, Int)]], ticket: List[Int]): Boolean = {
    ticket.forall(x => checkRules(rules, x))
  }

  def determineColumnNames(rules: Map[String, List[(Int, Int)]], tickets: List[List[Int]]): Map[Int, String] = {
    def helper(accum: Map[Int, String], known: Set[String], remaining: List[(Set[String], Int)]): Map[Int, String] = {
      val (k, r) = remaining.partition(_._1.size == 1)  // Find fields with only a single valid name
      if(k.isEmpty) accum
      else {
        val newKnown = k.map { case (s, i) => (i, s.head) }   // We know s has only one entry
        val nextKnown = newKnown.map(_._2).toSet ++ known
        val nextAccum = accum ++ newKnown.toMap
        val nextRem = r.map { case (s, i) => (s -- nextKnown, i) }

        helper(nextAccum, nextKnown, nextRem)
      }
    }

    val cs =
      tickets
        .map(_.map(x => rules.collect { case (n, r) if checkRule(r, x) => n}.toSet)) // All valid names for each field
        .transpose                                                                   // Turn rows into columns
        .map(_.reduce(_&_))                                                          // Intersect possible field names for each column
        .zipWithIndex                                                                // Add the index

    helper(Map.empty[Int, String], Set.empty[String], cs)
  }

  case class TicketData(rules: Map[String, List[(Int, Int)]], ticket: List[Int], nearby: List[List[Int]])

  val fieldRuleRegex = """(.*): (\d+)-(\d+) or (\d+)-(\d+)""".r

  def parseFieldRule(s: String): (String, List[(Int, Int)]) =
    s match {
      case fieldRuleRegex(n, a, b, c, d) => (n, List((a.toInt, b.toInt), (c.toInt, d.toInt)))
      case _ => throw new Exception("Invalid field rule")
    }

  def parseTicket(lines: List[String]): List[Int] =
    lines.drop(1).head.split(",").map(_.toInt).toList

  def parseNearby(lines: List[String]): List[List[Int]] =
    lines.drop(1).map(_.split(",").map(_.toInt).toList).toList

  def parseInput(lines: Iterator[String]): TicketData = {
    val rules = lines.takeWhile(!_.isEmpty).toList.map(parseFieldRule)
    val ticket = parseTicket(lines.takeWhile(!_.isEmpty).toList)
    val nearby = parseNearby(lines.takeWhile(!_.isEmpty).toList)

    TicketData(rules.toMap, ticket, nearby)
  }

  def readFile(f: String): TicketData =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day16.txt"

  def testData = """class: 1-3 or 5-7
                   |row: 6-11 or 33-44
                   |seat: 13-40 or 45-50
                   |
                   |your ticket:
                   |7,1,14
                   |
                   |nearby tickets:
                   |7,3,47
                   |40,4,50
                   |55,2,20
                   |38,6,12""".stripMargin.linesIterator

  def testData2 = """class: 0-1 or 4-19
                    |row: 0-5 or 8-19
                    |seat: 0-13 or 16-19
                    |
                    |your ticket:
                    |11,12,13
                    |
                    |nearby tickets:
                    |3,9,18
                    |15,1,5
                    |5,14,9""".stripMargin.linesIterator

}
