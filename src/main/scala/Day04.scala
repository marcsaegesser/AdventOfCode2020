package advent

/**
  * Maybe I'm missing something, but there isn't anything remotely
  * interesting about this puzzle.
  */
object Day04 {

  def run(): Unit = {
    val passports = parseInput(readFile(inputFile))
    println(s"Day04.part1 = ${part1(passports)}")
    println(s"Day04.part2 = ${part2(passports)}")
  }

  def part1(passports: List[Passport]): Int =
    passports.filter(hasRequiredFields).size

  def part2(passports: List[Passport]): Int =
    passports.filter(isValid).size

  type Passport = Map[String, String]

  def isValid(passport: Passport): Boolean = {
    hasRequiredFields(passport) && validateFields(passport)
  }

  val requiredFields = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")

  def hasRequiredFields(passport: Passport): Boolean =
    (requiredFields -- passport.keySet).isEmpty

  def validateFields(passport: Passport): Boolean =
    !passport.exists { case (f, v) => !isValidField(f, v) }

  def isValidField(name: String, value: String): Boolean =
    name match {
      case "byr" => validateRange(value.toInt, 1920, 2002)
      case "iyr" => validateRange(value.toInt, 2010, 2020)
      case "eyr" => validateRange(value.toInt, 2020, 2030)
      case "hgt" => validateHeight(value)
      case "hcl" => validateHairColor(value)
      case "ecl" => validateEyeColor(value)
      case "pid" => validatePID(value)
      case "cid" => true
    }

  def validateRange(value: Int, min: Int, max: Int): Boolean =
    value >= min && value <= max

  val heightRegex = """(\d+)(\w+)""".r

  @annotation.nowarn
  def validateHeight(value: String): Boolean = {
    try {
      val heightRegex(h, u) = value
      u match {
        case "cm" => validateRange(h.toInt, 150, 193)
        case "in" => validateRange(h.toInt, 59, 76)
        case _    => false
      }
    } catch {
      case _: Throwable => false
    }
  }

  val hairRegex = """#[0-9a-f]{6}""".r

  def validateHairColor(value: String): Boolean =
    hairRegex.matches(value)

  val validEyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def validateEyeColor(value: String): Boolean =
    validEyeColors.contains(value)

  val pidRegex = """\d{9}""".r

  def validatePID(value: String): Boolean =
    pidRegex.matches(value)

  def parseInput(lines: Iterator[String]): List[Passport] = {
    val lineRegex = """([^\s]+):([^\s]+)\s?""".r

    val (ps, m) =
      lines.foldLeft((List.empty[Map[String, String]], Map.empty[String, String])) { case ((ps, m), l) =>
        if(l.trim.isEmpty) (m +: ps, Map.empty[String, String])
        else               (ps, m ++ lineRegex.findAllMatchIn(l).map(m => (m.group(1), m.group(2))))
      }

    m +: ps
  }

  def readFile(f: String): Iterator[String] =
    io.Source.fromFile(f).getLines()

  val inputFile = "data/Day04.txt"

  def testData = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
                   |byr:1937 iyr:2017 cid:147 hgt:183cm
                   |
                   |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
                   |hcl:#cfa07d byr:1929
                   |
                   |hcl:#ae17e1 iyr:2013
                   |eyr:2024
                   |ecl:brn pid:760753108 byr:1931
                   |hgt:179cm
                   |
                   |hcl:#cfa07d eyr:2025 pid:166559648
                   |iyr:2011 ecl:brn hgt:59in""".stripMargin.linesIterator

  def invalidPassports = """eyr:1972 cid:100
                           |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
                           |
                           |iyr:2019
                           |hcl:#602927 eyr:1967 hgt:170cm
                           |ecl:grn pid:012533040 byr:1946
                           |
                           |hcl:dab227 iyr:2012
                           |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
                           |
                           |hgt:59cm ecl:zzz
                           |eyr:2038 hcl:74454a iyr:2023
                           |pid:3556412378 byr:2007""".stripMargin.linesIterator

  def validPassports = """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
                         |hcl:#623a2f
                         |
                         |eyr:2029 ecl:blu cid:129 byr:1989
                         |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
                         |
                         |hcl:#888785
                         |hgt:164cm byr:2001 iyr:2015 cid:88
                         |pid:545766238 ecl:hzl
                         |eyr:2022
                         |
                         |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""".stripMargin.linesIterator
}
