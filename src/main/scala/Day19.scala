package advent

object Day19 {

  def run(): Unit = {
    val (rules, data) = readFile(inputFile)
    println(s"Day19.part1 = ${part1(rules, data)}")
    println(s"Day19.part2 = ${part2(rules, data)}")
  }

  def part1(rules: Rules, data: List[String]): Int =
    data
      .map(matchesRule(rules, 0, _))
      .filter(identity)
      .size

  def part2(rules: Rules, data: List[String]): Int =
    data
      .map(matchesRule(updateRules(rules), 0, _))
      .filter(identity)
      .size

  def matchesRule(rules: Rules, id: Int, target: String): Boolean = {
    def helper(r: Int, s: String): List[String] = {
      rules(r) match {
        case Terminal(c) if s.startsWith(c.toString) => List(s.tail) // Prefix matches so continue with remaining characters
        case Terminal(_)                             => Nil          // No match
        case Internal(ls) =>
          ls.flatMap(l =>                         // For each sub-rule in the rule
              l.foldLeft(List(s)) {               // For each rule id in the sub-rule
                case (ts, i) =>                   // For each potential match
                  ts.flatMap(t => helper(i, t))   // Recurse to find potential matches for this rule id
              })
      }
    }

    helper(id, target).contains("")
  }

  def updateRules(rules: Rules): Rules =
    rules
      .updated(8,  Internal(List(List(42), List(42, 8))))
      .updated(11, Internal(List(List(42, 31), List(42, 11, 31))))

  type Rules = Map[Int, Rule]

  sealed trait Rule
  case class Internal(ls: List[List[Int]]) extends Rule
  case class Terminal(c: Char) extends Rule

  val ruleRegexA = """(\d+): ([^|]+) \| ([^|]+)""".r
  val ruleRegexB = """(\d+): (.*)""".r
  val ruleRegexC = """(\d+): (\w)""".r

  def parseRule(s: String): (Int, Rule) = {
    def parseList(s: String): List[Int] = s.trim.split(" ").toList.map(_.toInt)
    s.filterNot(_ == '"') match {
      case ruleRegexC(n, c)    => (n.toInt, Terminal(c.head))
      case ruleRegexA(n, a, b) => (n.toInt, Internal(List(parseList(a), parseList(b))))
      case ruleRegexB(n, a)    => (n.toInt, Internal(List(parseList(a))))
      case _                   => throw new Exception("Invalid input")
    }
  }

  def parseRules(input: List[String]): Rules =
    input.map(parseRule).toMap

  def parseInput(input: Iterator[String]): (Rules, List[String])  = {
    val rules = parseRules(input.takeWhile(!_.isEmpty).toList)
    val strings = input.toList
    (rules, strings)
  }

  def readFile(f: String): (Rules, List[String]) =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day19.txt"

  def testData = """0: 4 1 5
                   |1: 2 3 | 3 2
                   |2: 4 4 | 5 5
                   |3: 4 5 | 5 4
                   |4: "a"
                   |5: "b"
                   |
                   |ababbb
                   |bababa
                   |abbbab
                   |aaabbb
                   |aaaabbb""".stripMargin.linesIterator

  def testData2 = """42: 9 14 | 10 1
                    |9: 14 27 | 1 26
                    |10: 23 14 | 28 1
                    |1: "a"
                    |11: 42 31
                    |5: 1 14 | 15 1
                    |19: 14 1 | 14 14
                    |12: 24 14 | 19 1
                    |16: 15 1 | 14 14
                    |31: 14 17 | 1 13
                    |6: 14 14 | 1 14
                    |2: 1 24 | 14 4
                    |0: 8 11
                    |13: 14 3 | 1 12
                    |15: 1 | 14
                    |17: 14 2 | 1 7
                    |23: 25 1 | 22 14
                    |28: 16 1
                    |4: 1 1
                    |20: 14 14 | 1 15
                    |3: 5 14 | 16 1
                    |27: 1 6 | 14 18
                    |14: "b"
                    |21: 14 1 | 1 14
                    |25: 1 1 | 1 14
                    |22: 14 14
                    |8: 42
                    |26: 14 22 | 1 20
                    |18: 15 15
                    |7: 14 5 | 1 21
                    |24: 14 1
                    |
                    |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
                    |bbabbbbaabaabba
                    |babbbbaabbbbbabbbbbbaabaaabaaa
                    |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
                    |bbbbbbbaaaabbbbaaabbabaaa
                    |bbbababbbbaaaaaaaabbababaaababaabab
                    |ababaaaaaabaaab
                    |ababaaaaabbbaba
                    |baabbaaaabbaaaababbaababb
                    |abbbbabbbbaaaababbbbbbaaaababb
                    |aaaaabbaabaaaaababaa
                    |aaaabbaaaabbaaa
                    |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
                    |babaaabbbaaabaababbaabababaaab
                    |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin.linesIterator
}
