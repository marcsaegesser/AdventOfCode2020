package advent

object Day07 {

  def run(): Unit = {
    val rules = parseFile(inputFile)
    println(s"Day07.part1 = ${part1(rules)}")
    println(s"Day07.part2 = ${part2(rules)}")
  }

  def part1(rules: Rules): Int =
    rules.keys.toList
      .map(c => containsColor(rules, c, "shiny gold"))
      .count(identity)

  def part2(rules: Rules): Int =
    countBags(rules, "shiny gold")



  def containsColor(rules: Rules, bag: String, color: String): Boolean = {
    def helper(bags: List[(String, Int)]): Boolean = {
      bags match {
        case Nil                       => false
        case (c, _) :: _ if c == color => true   // Stop at the first match
        case (c, _) :: t               => helper(rules(c)) || helper(t)
      }
    }

    helper(rules(bag))
  }

  def countBags(rules: Rules, bag: String): Int = {
    def helper(bags: List[(String, Int)]): Int =
      bags match {
        case Nil         => 0
        case (c, n) :: t => (n + n * helper(rules(c))) + helper(t)
      }

    helper(rules(bag))
  }

  type BagColor = String
  type BagContents = List[(String, Int)]
  type Rules = Map[BagColor, BagContents]


  val ruleRegex = """(.*) bags contain (.*)""".r
  val rhsRegex = """\s*(?:(\d+)\s+(\w+\s\w+)\s(?:bag|bags)[,.])""".r

  def parseRules(lines: Iterator[String]): Rules =
    lines.map(parseRule).toMap

  @annotation.nowarn
  def parseRule(s: String): (BagColor, BagContents) = {
    val ruleRegex(lhs, rhs) = s
    val contents = rhsRegex.findAllMatchIn(rhs).map(m => (m.group(2), m.group(1).toInt)).toList

    (lhs.trim -> contents)
  }

  def parseFile(f: String): Rules =
    io.Source.fromFile(f)
      .getLines()
      .map(parseRule)
      .toMap

  val inputFile = "data/Day07.txt"

  def testData = """light red bags contain 1 bright white bag, 2 muted yellow bags.
                   |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
                   |bright white bags contain 1 shiny gold bag.
                   |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
                   |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
                   |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
                   |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
                   |faded blue bags contain no other bags.
                   |dotted black bags contain no other bags.""".stripMargin.linesIterator
}
