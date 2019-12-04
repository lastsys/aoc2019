package com.lastsys.aoc2019.day04

import com.lastsys.aoc2019.util.AocTask

import scala.util.matching.Regex

object Day04 extends AocTask {
  override def run(): Unit = {
    val part1 = countPasswords1(256310, 732736)
    println(s"Day04 :: Part1 = $part1")

    val part2 = countPasswords2(256310, 732736)
    println(s"Day04 :: Part2 = $part2")
  }

  val pattern1: Regex = """(\d)\1""".r

  /** Count valid passwords for the given range.
   *
   *  @param start start of range
   *  @param end end of range
   *  @return password
   */
  def countPasswords1(start: Int, end: Int): Int = {
    (start to end).count { pw =>
      checkNumbersNonDecreasing(pw) && checkRepeatingNumbers(pw)
    }
  }

  /** Count valid passwords for the given range part 2.
   *
   *  @param start start of range
   *  @param end end of range
   *  @return password
   */
  def countPasswords2(start: Int, end: Int): Int = {
    (start to end).count { pw =>
      checkNumbersNonDecreasing(pw) && checkExactPair(pw)
    }
  }

  /** Check if numbers are non-decreasing. */
  def checkNumbersNonDecreasing(pw: Int): Boolean = {
    val s = pw.toString
    val digits = s.map(_.asDigit)
    digits == digits.sorted
  }

  /** Check if there is at least two repeating numbers in sequence. */
  def checkRepeatingNumbers(pw: Int): Boolean = {
    pattern1.findFirstIn(pw.toString).nonEmpty
  }

  /** Check if there is at least one pair of exactly two same numbers in sequence. */
  def checkExactPair(pw: Int): Boolean = {
    val s = pw.toString
    (0 to 9).exists { i =>
      // Unable to express regex for generic character due to lookbehind.
      val p = s"""(?<!$i)$i{2}(?!$i)""".r
      p.findFirstIn(s).nonEmpty
    }
  }
}
