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
       |...##""".stripMargin.split("\n").map(_.stripTrailing), Point(3, 4), 8),
    ("""......#.#.
       |#..#.#....
       |..#######.
       |.#.#.###..
       |.#..#.....
       |..#....#.#
       |#..#....#.
       |.##.#..###
       |##...#..#.
       |.#....####""".stripMargin.split("\n").map(_.stripTrailing), Point(5, 8), 33),
    ("""#.#...#.#.
       |.###....#.
       |.#....#...
       |##.#.#.#.#
       |....#.#.#.
       |.##..###.#
       |..#...##..
       |..##....##
       |......#...
       |.####.###.""".stripMargin.split("\n").map(_.stripTrailing), Point(1, 2), 35),
    (""".#..#..###
       |####.###.#
       |....###.#.
       |..###.##.#
       |##.##.#.#.
       |....###..#
       |..#.#..#.#
       |#..#.#.###
       |.##...##.#
       |.....#.#..""".stripMargin.split("\n").map(_.stripTrailing), Point(6, 3), 41),
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
       |###.##.####.##.#..##""".stripMargin.split("\n").map(_.stripTrailing), Point(11, 13), 210)
  )

  property("example 1 cases should be valid") {
    forAll (examples1) { (map: Array[String], position: Point, count: Int) =>
      Day10.findBestAsteroid(Day10.parseMap(map)) shouldBe (position, count)
    }
  }

  val example2_map = """.#....#####...#..
                       |##...##.#####..##
                       |##...#...#.#####.
                       |..#.....#...###..
                       |..#.#.....#....##""".stripMargin.split("\n").map(_.stripTrailing)

  val example2_2nd = """........#.....#..
                       |..........#.....#
                       |.................
                       |........#....##..
                       |.................""".stripMargin.split("\n").map(_.stripTrailing)

  val example2_3rd = """.................
                       |.................
                       |.................
                       |........#.....#..
                       |.................""".stripMargin.split("\n").map(_.stripTrailing)

  val example2_first_revolution = Seq(
    Point(8, 1), Point(9, 0), Point(9, 1), Point(10, 0), Point(9, 2), Point(11, 1), Point(12, 1), Point(11, 2), Point(15, 1),
    Point(12, 2), Point(13, 2), Point(14, 2), Point(15, 2), Point(12, 3), Point(16, 4), Point(15, 4), Point(10, 4), Point(4, 4),
    Point(2, 4), Point(2, 3), Point(0, 2), Point(1, 2), Point(0, 1), Point(1, 1), Point(5, 2), Point(1, 0), Point(5, 1),
    Point(6, 1), Point(6, 0), Point(7, 0))

  val example2_second_revolution = Seq(
    Point(8, 0), Point(10, 1), Point(14, 0), Point(16, 1), Point(13, 3)
  )

  val example2_third_revolution = Seq(Point(14, 3))

  property("example 2 first revolution vaporization order should be correct") {
    val m = Day10.parseMap(example2_map)
    val visible = Day10.findVisible(m, Point(8, 3))
    visible shouldBe example2_first_revolution.toSet
    val sorted = Day10.sortPointsByAngle(visible, Point(8, 3))
    sorted shouldBe example2_first_revolution
  }

  property("example 2 second revolution vaporization order should be correct") {
    val m = Day10.parseMap(example2_map)
    val m2 = Day10.parseMap(example2_2nd)
    val visible = Day10.findVisible(m, Point(8, 3))
    visible shouldBe example2_first_revolution.toSet
    val mReduced = m -- visible
    mReduced shouldBe m2
    val visible2 = Day10.findVisible(m2, Point(8, 3))
    val sorted2 = Day10.sortPointsByAngle(visible2, Point(8, 3))
    sorted2 shouldBe example2_second_revolution
  }

  property("example 2 third revolution vaporization order should be correct") {
    val m3 = Day10.parseMap(example2_3rd)
    val visible = Day10.findVisible(m3, Point(8,3))
    val sorted = Day10.sortPointsByAngle(visible, Point(8, 3))
    sorted shouldBe example2_third_revolution
  }

  property("example 2 eliminate all in correct order") {
    val totalOrder = example2_first_revolution ++ example2_second_revolution ++ example2_third_revolution
    val m = Day10.parseMap(example2_map)
    val eliminationOrder = Day10.eliminateInOrder(m, Point(8, 3))
    eliminationOrder shouldBe totalOrder
  }
}
