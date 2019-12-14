package com.lastsys.aoc2019.day09

import com.lastsys.aoc2019.util.IntCode
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

import scala.collection.immutable.ArraySeq

class Day09Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1: TableFor2[ArraySeq[String], String] =
    Table(
      ("program", "output"),
      (ArraySeq[String]("109", "1", "204", "-1", "1001", "100", "1", "100", "1008", "100", "16", "101", "1006", "101", "0", "99"), "1091204-1100110011001008100161011006101099"),
      (ArraySeq[String]("1102", "34915192", "34915192", "7", "4", "7", "99", "0"), "1219070632396864"),
      (ArraySeq[String]("104", "1125899906842624", "99"), "1125899906842624")
    )

  property("example 1 cases should be valid") {
    forAll (examples1) { (program: ArraySeq[String], output: String) =>
      var vm = IntCode(IntCode.convertFromString(program)).putInput(0)
      while (!vm.halt) vm = vm.step
      vm.output.mkString("").toString shouldBe output
    }
  }
}
