package com.lastsys.aoc2019.day01

import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.{Matchers, PropSpec}

class Day01Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1: TableFor2[Int, Int] =
    Table(
      ("mass", "fuel"),
      (12, 2),
      (14, 2),
      (1969, 654),
      (100756, 33583)
    )

  property("all example cases for part 1 should be fulfilled") {
    forAll (examples1) { (mass: Int, fuel: Int) =>
      Day01.calcFuelFromMass(mass) shouldEqual fuel
    }
  }

  val examples2: TableFor2[Int, Int] =
    Table(
      ("mass", "fuel"),
      (14, 2),
      (1969, 966),
      (100756, 50346)
    )

  property("all example cases for part 2 should be fulfilled") {
    forAll (examples2) { (mass: Int, fuel: Int) =>
      Day01.calcCompensatedFuelFromMass(mass) shouldEqual fuel
    }
  }
}