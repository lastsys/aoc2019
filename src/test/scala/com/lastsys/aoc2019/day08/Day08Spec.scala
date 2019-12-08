package com.lastsys.aoc2019.day08

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.TableDrivenPropertyChecks

class Day08Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  property("example 2 should be valid") {
    val raw = "0222112222120000"
    val layers = Day08.parseImage(raw, 2, 2)
    layers shouldBe Seq(
      Seq(0, 2, 2, 2),
      Seq(1, 1, 2, 2),
      Seq(2, 2, 1, 2),
      Seq(0, 0, 0 ,0)
    )
    Day08.flatten(layers) shouldBe Seq(0, 1, 1, 0)
  }
}
