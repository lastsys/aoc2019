package com.lastsys.aoc2019.day07

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3}

import scala.collection.immutable.ArraySeq

class Day07Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1: TableFor3[ArraySeq[Int], Int, Seq[Int]] =
    Table(
      ("program", "thruster", "sequence"),
      (ArraySeq(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0), 43210, Seq(4, 3, 2, 1, 0)),
      (ArraySeq(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
        101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0), 54321, Seq(0, 1, 2, 3, 4)),
      (ArraySeq(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
        1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0), 65210, Seq(1, 0, 4, 3, 2))
    )

    property("example 1 max signals should be valid") {
      forAll(examples1) { (program: ArraySeq[Int], thruster: Int, sequence: Seq[Int]) =>
        Day07.thrust(program, sequence) shouldBe thruster
      }
    }

    property("all example cases for part 1 should be fulfilled") {
      forAll(examples1) { (program: ArraySeq[Int], thruster: Int, sequence: Seq[Int]) =>
        Day07.maximizeThruster(program) shouldBe (thruster, sequence)
      }
    }
}
