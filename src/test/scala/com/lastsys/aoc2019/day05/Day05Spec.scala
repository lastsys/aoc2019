package com.lastsys.aoc2019.day05

import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2, TableFor3}
import org.scalatest.{Matchers, PropSpec}

import scala.collection.immutable.ArraySeq

class Day05Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1: TableFor3[ArraySeq[Int], ArraySeq[Int], String] =
    Table(
      ("before", "after", "output"),
      (ArraySeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), ArraySeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50), ""),
      (ArraySeq(1, 0, 0, 0, 99), ArraySeq(2, 0, 0, 0, 99), ""),
      (ArraySeq(2, 3, 0, 3, 99), ArraySeq(2, 3, 0, 6, 99), ""),
      (ArraySeq(2, 4, 4, 5, 99, 0), ArraySeq(2, 4, 4, 5, 99, 9801), ""),
      (ArraySeq(1, 1, 1, 4, 99, 5, 6, 0, 99), ArraySeq(30, 1, 1, 4, 2, 5, 6, 0, 99), ""),
      // New cases
      (ArraySeq(3, 0, 4, 0, 99), ArraySeq(1, 0, 4, 0, 99), "1"),
      (ArraySeq(1002, 4, 3, 4, 33), ArraySeq(1002, 4, 3, 4, 99), ""),
      (ArraySeq(1101, 100, -1, 4, 0), ArraySeq(1101, 100, -1, 4, 99), "")
    )

  property("all example cases for part 1 should be fulfilled") {
    forAll(examples1) { (before: ArraySeq[Int], after: ArraySeq[Int], output: String) =>
      Day05.execute(before)(1) shouldEqual(after, output)
    }
  }

  val examples2: TableFor3[ArraySeq[Int], Int, String] =
    Table(
      ("before", "input", "output"),
      (ArraySeq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 0, "0"),
      (ArraySeq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 8, "1"),

      (ArraySeq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 0, "1"),
      (ArraySeq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 8, "0"),

      (ArraySeq(3, 3, 1108, -1, 8, 3, 4, 3, 99), 0, "0"),
      (ArraySeq(3, 3, 1108, -1, 8, 3, 4, 3, 99), 8, "1"),

      (ArraySeq(3, 3, 1107, -1, 8, 3, 4, 3, 99), 0, "1"),
      (ArraySeq(3, 3, 1107, -1, 8, 3, 4, 3, 99), 8, "0"),

      (ArraySeq(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 0, "0"),
      (ArraySeq(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 1, "1"),

      (ArraySeq(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), 0, "0"),
      (ArraySeq(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), 1, "1"),

      (ArraySeq(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 7, "999"),
      (ArraySeq(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 8, "1000"),
      (ArraySeq(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 9, "1001")
    )

  property("all example cases for part 2 should be fulfilled") {
    forAll(examples2) { (before: ArraySeq[Int], input: Int, output: String) =>
      Day05.execute(before)(input)._2 shouldEqual output
    }
  }
}
