package com.lastsys.aoc2019.day04

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class Day04Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1 = {
    Table(
      ("pw", "nonDecreasing", "pair"),
      (111111, true, true),
      (223450, false, true),
      (123789, true, false)
    )
  }

  property("all example cases for part 1 should be fulfilled") {
    forAll(examples1) { (pw: Int, nonDecreasing: Boolean, pair: Boolean) =>
      Day04.checkNumbersNonDecreasing(pw) shouldEqual nonDecreasing
      Day04.checkRepeatingNumbers(pw) shouldEqual pair
    }
  }

  val examples2 = {
    Table(
      ("pw", "nonDecreasing", "exactPair"),
      (111122, true, true),
      (111223, true, true),
      (123444, true, false),
      (112233, true, true)
    )
  }

  property("all example cases for part 2 should be fulfilled") {
    forAll(examples2) { (pw: Int, nonDecreasing: Boolean, exactPair: Boolean) =>
      Day04.checkNumbersNonDecreasing(pw) shouldEqual nonDecreasing
      Day04.checkExactPair(pw) shouldEqual exactPair
    }
  }
}
