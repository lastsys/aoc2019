package com.lastsys.aoc2019.day02

import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.{Matchers, PropSpec}

import scala.collection.immutable.ArraySeq

class Day02Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1: TableFor2[ArraySeq[Int], ArraySeq[Int]] =
    Table(
      ("before", "after"),
      (ArraySeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), ArraySeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)),
      (ArraySeq(1, 0, 0, 0, 99), ArraySeq(2, 0, 0, 0, 99)),
      (ArraySeq(2, 3, 0, 3, 99), ArraySeq(2, 3, 0, 6, 99)),
      (ArraySeq(2, 4, 4, 5, 99, 0), ArraySeq(2, 4, 4, 5, 99, 9801)),
      (ArraySeq(1, 1, 1, 4, 99, 5, 6, 0, 99), ArraySeq(30, 1, 1, 4, 2, 5, 6, 0, 99))
    )

  property("all example cases for part 1 should be fulfilled") {
    forAll (examples1) { (before: ArraySeq[Int], after: ArraySeq[Int]) =>
      Day02.execute(before, 0) shouldEqual after
    }
  }
}
