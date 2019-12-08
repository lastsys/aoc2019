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
      Day07.maximizeThruster(program) shouldBe(thruster, sequence)
    }
  }

  val examples2: TableFor3[ArraySeq[Int], Int, ArraySeq[Int]] =
    Table(
      ("program", "thruster", "sequence"),
      (ArraySeq(3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
        27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5), 139629729, ArraySeq(9, 8, 7, 6, 5)),
      (ArraySeq(3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
        -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
        53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10), 18216, ArraySeq(9, 7, 8, 5, 6))
    )

  property("example 2 max signals should be valid") {
    forAll(examples2) { (program: ArraySeq[Int], thruster: Int, sequence: Seq[Int]) =>
      Day07.thrustWithFeedback(program, sequence) shouldBe thruster
    }
  }

  property("all example cases for part 2 should be fulfilled") {
    forAll(examples2) { (program: ArraySeq[Int], thruster: Int, sequence: Seq[Int]) =>
      Day07.maximizeThruster(program, 5, 9, true) shouldBe(thruster, sequence)
    }
  }
}
