package com.lastsys.aoc2019.day03

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3}

class Day03Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1: TableFor3[String, String, Int] =
    Table(
      ("path1", "path2", "distance"),
      ("R8,U5,L5,D3", "U7,R6,D4,L4", 6),
      ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 159),
      ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)
    )

  property("all example cases for part 1 should be fulfilled") {
    forAll (examples1) { (path1: String, path2: String, distance: Int) =>
      val p1 = Day03.parsePath(path1)
      val p2 = Day03.parsePath(path2)
      Day03.distanceToClosestIntersection(p1, p2) shouldEqual distance
    }
  }

  val examples2 =
    Table(
      ("path1", "path2", "steps"),
      ("R8,U5,L5,D3", "U7,R6,D4,L4", 30),
      ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 610),
      ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)
    )

  property("all examples cases for part 2 should be fulfilled") {
    forAll (examples2) { (path1: String, path2: String, steps: Int) =>
      val p1 = Day03.parsePath(path1)
      val p2 = Day03.parsePath(path2)
      Day03.closestIntersectionBySteps(p1, p2) shouldEqual steps
    }
  }
}
