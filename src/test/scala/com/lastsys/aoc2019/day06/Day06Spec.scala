package com.lastsys.aoc2019.day06

import org.scalatest.{Matchers, PropSpec}

class Day06Spec extends PropSpec with Matchers {
  val example1 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L".split("\n")

  property("example 1 should be fulfilled") {
    val orbits = Day06.parseOrbits(example1)
    Day06.countOrbits(orbits) shouldBe 42
  }

  val example2 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN".split("\n")

  property("example 2 should be fulfilled") {
    val orbits = Day06.parseOrbits(example2)
    Day06.shortestPath(orbits)("YOU", "SAN") shouldBe 4
  }
}
