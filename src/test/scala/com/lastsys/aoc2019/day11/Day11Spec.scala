package com.lastsys.aoc2019.day11

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.TableDrivenPropertyChecks

class Day11Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  property("turtle should move properly") {
    Turtle(Point(0, 0), 0, -1).turnLeft shouldBe Turtle(Point(0, 0), -1, 0)
    Turtle(Point(0, 0), 0, -1).turnRight shouldBe Turtle(Point(0, 0), 1, 0)
  }
}
