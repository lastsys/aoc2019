package com.lastsys.aoc2019.util

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

class IntCodeSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examplesFromDay02 =
    Table(
      ("before", "after"),
      (ArraySeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), ArraySeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)),
      (ArraySeq(1, 0, 0, 0, 99), ArraySeq(2, 0, 0, 0, 99)),
      (ArraySeq(2, 3, 0, 3, 99), ArraySeq(2, 3, 0, 6, 99)),
      (ArraySeq(2, 4, 4, 5, 99, 0), ArraySeq(2, 4, 4, 5, 99, 9801)),
      (ArraySeq(1, 1, 1, 4, 99, 5, 6, 0, 99), ArraySeq(30, 1, 1, 4, 2, 5, 6, 0, 99))
    )

  property("examples from day 2 should work") {
    forAll (examplesFromDay02) { (before: ArraySeq[Int], after: ArraySeq[Int]) =>
      var vm = IntCode(IntCode.convertFromInt(before))
      while (!vm.halt) {
        vm = vm.step
      }
      vm.mem shouldBe IntCode.convertFromInt(after)
    }
  }

  val examplesFromDay05Part1 =
    Table(
      ("before", "after"),
      (ArraySeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), ArraySeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)),
      (ArraySeq(1, 0, 0, 0, 99), ArraySeq(2, 0, 0, 0, 99)),
      (ArraySeq(2, 3, 0, 3, 99), ArraySeq(2, 3, 0, 6, 99)),
      (ArraySeq(2, 4, 4, 5, 99, 0), ArraySeq(2, 4, 4, 5, 99, 9801)),
      (ArraySeq(1, 1, 1, 4, 99, 5, 6, 0, 99), ArraySeq(30, 1, 1, 4, 2, 5, 6, 0, 99)),
      // New cases
      (ArraySeq(3, 0, 4, 0, 99), ArraySeq(1, 0, 4, 0, 99)),
      (ArraySeq(1002, 4, 3, 4, 33), ArraySeq(1002, 4, 3, 4, 99)),
      (ArraySeq(1101, 100, -1, 4, 0), ArraySeq(1101, 100, -1, 4, 99))

    )

  property("examples from day 5 part 1 should work") {
    forAll (examplesFromDay05Part1) { (before: ArraySeq[Int], after: ArraySeq[Int]) =>
      val mem1 = IntCode.convertFromInt(before)
      val mem2 = IntCode.convertFromInt(after)
      var vm = IntCode(mem1).putInput(1)
      while (!vm.halt) {
        vm = vm.step
      }
      vm.mem shouldBe mem2
    }
  }

  val examplesFromDay05Part2 =
    Table(
      ("before", "input", "output"),
      (ArraySeq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 0, 0),
      (ArraySeq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 8, 1),

      (ArraySeq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 0, 1),
      (ArraySeq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 8, 0),

      (ArraySeq(3, 3, 1108, -1, 8, 3, 4, 3, 99), 0, 0),
      (ArraySeq(3, 3, 1108, -1, 8, 3, 4, 3, 99), 8, 1),

      (ArraySeq(3, 3, 1107, -1, 8, 3, 4, 3, 99), 0, 1),
      (ArraySeq(3, 3, 1107, -1, 8, 3, 4, 3, 99), 8, 0),

      (ArraySeq(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 0, 0),
      (ArraySeq(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9), 1, 1),

      (ArraySeq(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), 0, 0),
      (ArraySeq(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1), 1, 1),

      (ArraySeq(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 7, 999),
      (ArraySeq(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 8, 1000),
      (ArraySeq(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 9, 1001)
    )

    property("all example cases for day 5 part 2 should work") {
      forAll (examplesFromDay05Part2) { (before: ArraySeq[Int], input: Int, output: Int) =>
        val mem1 = IntCode.convertFromInt(before)
        var vm = IntCode(mem1).putInput(input)
        while (!vm.halt) {
          vm = vm.step
        }
        vm.output.head shouldBe output
      }
    }

  val examplesFromDay09 = Table(
    ("program", "output"),
    (ArraySeq[String]("109", "1", "204", "-1", "1001", "100", "1", "100", "1008", "100", "16", "101", "1006", "101", "0", "99"), "1091204-1100110011001008100161011006101099"),
    (ArraySeq[String]("1102", "34915192", "34915192", "7", "4", "7", "99", "0"), "1219070632396864"),
    (ArraySeq[String]("104", "1125899906842624", "99"), "1125899906842624")
  )

  property("all example cases for day 9 should work") {
    forAll (examplesFromDay09) { (program: ArraySeq[String], output: String) =>
      val mem1 = IntCode.convertFromString(program)
      var vm = IntCode(mem1).putInput(0)
      while (!vm.halt) {
        vm = vm.step
      }
      vm.output.mkString("") shouldBe output
    }
  }
}
