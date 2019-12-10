package com.lastsys.aoc2019.day10

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3}

class Day10Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples1: TableFor3[Array[String], Point, Int] = Table(
    ("map", "position", "count"),
    (""".#..#
       |.....
       |#####
       |....#
       |...##""".stripMargin.split("\n"), Point(3, 4), 8),
    ("""......#.#.
       |#..#.#....
       |..#######.
       |.#.#.###..
       |.#..#.....
       |..#....#.#
       |#..#....#.
       |.##.#..###
       |##...#..#.
       |.#....####""".stripMargin.split("\n"), Point(5, 8), 33),
    ("""#.#...#.#.
       |.###....#.
       |.#....#...
       |##.#.#.#.#
       |....#.#.#.
       |.##..###.#
       |..#...##..
       |..##....##
       |......#...
       |.####.###.""".stripMargin.split("\n"), Point(1, 2), 35),
    (""".#..#..###
       |####.###.#
       |....###.#.
       |..###.##.#
       |##.##.#.#.
       |....###..#
       |..#.#..#.#
       |#..#.#.###
       |.##...##.#
       |.....#.#..""".stripMargin.split("\n"), Point(6, 3), 41),
    (""".#..##.###...#######
       |##.############..##.
       |.#.######.########.#
       |.###.#######.####.#.
       |#####.##.#.##.###.##
       |..#####..#.#########
       |####################
       |#.####....###.#.#.##
       |##.#################
       |#####.##.###..####..
       |..######..##.#######
       |####.##.####...##..#
       |.#####..#.######.###
       |##...#.##########...
       |#.##########.#######
       |.####.#.###.###.#.##
       |....##.##.###..#####
       |.#.#.###########.###
       |#.#.#.#####.####.###
       |###.##.####.##.#..##""".stripMargin.split("\n"), Point(11, 13), 210)
  )

  property("example 1 cases should be valid") {
    forAll (examples1) { (map: Array[String], position: Point, count: Int) =>
      Day10.findBestAsteroid(Day10.parseMap(map)) shouldBe (position, count)
    }
  }
}
