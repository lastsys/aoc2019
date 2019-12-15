package com.lastsys.aoc2019.day12

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.TableDrivenPropertyChecks
import scala.util.chaining._

class Day12Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val example1 = """<x=-1, y=0, z=2>
                   |<x=2, y=-10, z=-7>
                   |<x=4, y=-8, z=8>
                   |<x=3, y=5, z=-1>""".stripMargin

  property("parser should be able to parse example") {
    val coords = example1.split("\n").map(_.stripTrailing)
    Day12.coordinateParser(coords(0)) shouldBe Vector3(-1, 0, 2)
    Day12.coordinateParser(coords(1)) shouldBe Vector3(2, -10, -7)
    Day12.coordinateParser(coords(2)) shouldBe Vector3(4, -8, 8)
    Day12.coordinateParser(coords(3)) shouldBe Vector3(3, 5, -1)
  }

  property("parser should be able to parse to Moons") {
    val coords = example1.split("\n").map(_.stripTrailing)
    val moons = Day12.parseMoons(coords).toArray
    moons(0) shouldBe Moon(Vector3(-1, 0, 2), Vector3(0, 0, 0))
    moons(1) shouldBe Moon(Vector3(2, -10, -7), Vector3(0, 0, 0))
    moons(2) shouldBe Moon(Vector3(4, -8, 8), Vector3(0, 0, 0))
    moons(3) shouldBe Moon(Vector3(3, 5, -1), Vector3(0, 0, 0))
  }

  property("part 1 simulation should work") {
    val coords = example1.split("\n").map(_.stripTrailing)
    val moons = Day12.parseMoons(coords)

    val step1 = moons.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    step1(0) shouldBe Moon(Vector3(2, -1, 1), Vector3(3, -1, -1))
    step1(1) shouldBe Moon(Vector3(3, -7, -4), Vector3(1, 3, 3))
    step1(2) shouldBe Moon(Vector3(1, -7, 5), Vector3(-3, 1, -3))
    step1(3) shouldBe Moon(Vector3(2, 2, 0), Vector3(-1, -3, 1))

    val step2 = step1.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    step2(0) shouldBe Moon(Vector3(5, -3, -1), Vector3(3, -2, -2))
    step2(1) shouldBe Moon(Vector3(1, -2, 2), Vector3(-2, 5, 6))
    step2(2) shouldBe Moon(Vector3(1, -4, -1), Vector3(0, 3, -6))
    step2(3) shouldBe Moon(Vector3(1, -4, 2), Vector3(-1, -6, 2))

    val step3 = step2.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    val step4 = step3.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    val step5 = step4.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    val step6 = step5.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    val step7 = step6.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    val step8 = step7.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    val step9 = step8.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    val step10 = step9.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)

    step10(0) shouldBe Moon(Vector3(2, 1, -3), Vector3(-3, -2, 1))
    step10(1) shouldBe Moon(Vector3(1, -8, 0), Vector3(-1, 1, 3))
    step10(2) shouldBe Moon(Vector3(3, -6, 1), Vector3(3, 2, -3))
    step10(3) shouldBe Moon(Vector3(2, 0, 4), Vector3(1, -1, -1))
  }

  property("part 1 energy example") {
    val coords = example1.split("\n").map(_.stripTrailing)
    val moons = Day12.parseMoons(coords)

    val step10 = (0 until 10).foldLeft(moons) { (moons, _) =>
      moons.pipe(Day12.applyGravity).pipe(Day12.applyVelocity)
    }

    step10(0) shouldBe Moon(Vector3(2, 1, -3), Vector3(-3, -2, 1))
    step10(1) shouldBe Moon(Vector3(1, -8, 0), Vector3(-1, 1, 3))
    step10(2) shouldBe Moon(Vector3(3, -6, 1), Vector3(3, 2, -3))
    step10(3) shouldBe Moon(Vector3(2, 0, 4), Vector3(1, -1, -1))

    val pot = step10.map(_.pot)
    val kin = step10.map(_.kin)
    val tot = step10.map(_.tot)
    pot shouldBe Seq(6, 9, 10, 6)
    kin shouldBe Seq(6, 5, 8, 3)
    tot shouldBe Seq(36, 45, 80, 18)
    tot.sum shouldBe 179
  }
}
