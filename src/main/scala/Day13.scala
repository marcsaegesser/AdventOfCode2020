package advent

import math.Integral.Implicits._

object Day13 {

  def run(): Unit = {
    println(s"Day13.part1 = ${part1(readFile(inputFile))}")
    println(s"Day13.part2 = ${part2(readFile2(inputFile))}")
  }

  def part1(input: Schedule) = {
    val (a, b) = input.schedule.map(s => (s, checkSchedule(input.time, s))).minBy(_._2)
    a * b
  }

  def part2(input: List[(BigInt, BigInt)]): BigInt = {
    val (a, b) = chineseRemainder(input.map { case (i, b) => (b-i, b) })
    (a+b) % b
  }

  // I admit, I looked up the Chinese remainder theorem thing. Number theory makes my eyes glaze over.
  def chineseRemainder(input: List[(BigInt, BigInt)]): (BigInt, BigInt) = {
    input.foldLeft((BigInt(0), BigInt(1))) { case ((r1, m1), (r2, m2)) =>
      val r = r2 + m2 *(r1 - r2) * modularInverse(m2, m1)
      val m = m2 * m1
      (r.mod(m), m)
    }
  }

  def modularInverse(x: BigInt, m: BigInt): BigInt = {
    val (_, i, _) = gcdExtended(x, m)
    i % m
  }

  def gcdExtended(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
    if(a == 0) (b, 0, 1)
    else {
      val (gcd, x, y) = gcdExtended(b % a, a)
      (gcd, y - (b / a) * x, x)
    }
  }

  def checkSchedule(t: Int, s: Int): Int = {
    val (_, r) = t /% s
    s - r
  }

  case class Schedule(time: Int, schedule: List[Int])

  val inputFile = "data/Day13.txt"

  def parseSchedule(s: String): List[Int] =
    s.trim.split(",").toList
      .filterNot(_ == "x")
      .map(_.toInt)

  def parseFullSchedule(input: String): List[(BigInt, BigInt)] =
    input.trim.split(",").toList
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map { case (t, i) =>  (BigInt(i), BigInt(t.toInt))}


  def parseInput(input: Iterator[String]): Schedule = {
    input.toList match {
      case l1 :: l2 :: Nil => Schedule(l1.toInt, parseSchedule(l2))
      case _ => throw new Exception("Invalid input")
    }
  }

  def parseInput2(input: Iterator[String]): List[(BigInt, BigInt)] = {
    input.toList match {
      case _ :: l2 :: Nil => parseFullSchedule(l2)
      case _ => throw new Exception("Invalid input")
    }
  }

  def readFile(f: String): Schedule = {
    parseInput(io.Source.fromFile(f).getLines())
  }

  def readFile2(f: String): List[(BigInt, BigInt)] = {
    parseInput2(io.Source.fromFile(f).getLines())
  }

  def testData = """939
                   |7,13,x,x,59,x,31,19""".stripMargin.linesIterator
}
