package advent

object Day21 {

  type Ingredient = String
  type Allergen = String

  def part1(rules: List[Rule]) = {
    val allergenMap = buildMap(rules)
    val possibleAllergens = allergenMap.values.flatten.toSet
    val allIngredients = rules.foldLeft(Set.empty[Ingredient]) { case (s, r) => s ++ r.is.toSet }
    val safe = allIngredients -- possibleAllergens

    rules.map(_.is).foldLeft(0) { case (accum, l) => accum + (l.toSet & safe).size }
  }

  def part2(rules: List[Rule]) = {
    val allergenMap = buildMap(rules)
    val allergens = determineAllergens(allergenMap)
    allergens.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  def determineAllergens(allergenMap: Map[Allergen, Set[Ingredient]]) = {
    def helper(accum: Map[Allergen, Ingredient], known: Set[Ingredient], remaining: List[(Allergen, Set[Ingredient])]) : Map[Allergen, Ingredient] = {
      val (k, r) = remaining.partition(_._2.size == 1)
      if(k.isEmpty) accum
      else {
        val newKnown = k.map { case (a, is) => (a, is.head) }
        val nextKnown = newKnown.map(_._2).toSet ++ known
        val nextAccum = accum ++ newKnown.toMap
        val nextRem = r.map { case (a, is) => (a,  is -- nextKnown) }

        helper(nextAccum, nextKnown, nextRem)
      }
    }

    helper(Map.empty, Set.empty, allergenMap.toList)
  }

  case class Rule(is: List[Ingredient], as: List[Allergen])

  def buildMap(rules: List[Rule]): Map[Allergen, Set[Ingredient]] = {
    rules.foldLeft(Map.empty[Allergen, Set[Ingredient]]) { case (m, r) =>
      r.as.foldLeft(m) { case (m, a) =>
        m.updatedWith(a) {
          case Some(is) => Some(is & r.is.toSet)
          case None     => Some(r.is.toSet)
        }
      }
    }
  }


  val lineRegex = """(.*) \(contains (.*)\)""".r

  @annotation.nowarn
  def parseLine(line: String): Rule = {
    val lineRegex(is, as) = line
    Rule(is.split(" ").map(_.trim).toList, as.split(",").map(_.trim).toList)
  }

  def parseInput(lines: Iterator[String]): List[Rule] =
    lines.map(parseLine).toList

  def readFile(f: String): List[Rule] =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day21.txt"

  def testData = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                   |trh fvjkl sbzzf mxmxvkd (contains dairy)
                   |sqjhc fvjkl (contains soy)
                   |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin.linesIterator
}
