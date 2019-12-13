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
}
