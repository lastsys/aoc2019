package com.lastsys.aoc2019.day12

import com.lastsys.aoc2019.util.{AocTask, loadResource}
import scala.util.chaining._

case class Vector3(x: Int, y: Int, z: Int) {
  def +(other: Vector3): Vector3 = Vector3(x + other.x, y + other.y, z + other.z)
  def -(other: Vector3): Vector3 = Vector3(x - other.x, y - other.y, z - other.z)
}

object Vector3 {
  def zero: Vector3 = Vector3(0, 0, 0)
}

case class Moon(p: Vector3, v: Vector3) {
  def diff(other: Moon): Vector3 = {
    val delta = p - other.p
    Vector3(if (delta.x < 0) -1 else if (delta.x > 0) 1 else 0,
      if (delta.y < 0) -1 else if (delta.y > 0) 1 else 0,
      if (delta.z < 0) -1 else if (delta.z > 0) 1 else 0)
  }
  def pot: Int = Math.abs(p.x) + Math.abs(p.y) + Math.abs(p.z)
  def kin: Int = Math.abs(v.x) + Math.abs(v.y) + Math.abs(v.z)
  def tot: Int = pot * kin
}

object Day12 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day12.txt")

    input.foreach { data =>
      val moons = parseMoons(data)
      val step1000 = (0 until 1000).foldLeft(moons) { (moons, _) =>
        moons.pipe(applyGravity).pipe(applyVelocity)
      }
      val tot = step1000.map(_.tot).sum
      println(s"Day12 :: Part1 = $tot")
    }
  }

  def parseMoons(rawMoons: Seq[String]): Seq[Moon] = {
    rawMoons
      .map(coordinateParser)
      .map(p => Moon(p, Vector3.zero))
  }

  def coordinateParser(rawCoordinate: String): Vector3 = {
    import fastparse._, NoWhitespace._
    def number[_: P]: P[Unit] = CharIn("0-9").rep

    def coordinate[_: P]: P[Vector3] = P(
      "<x=" ~ ("-".? ~ number).! ~ ", " ~
        "y=" ~ ("-".? ~ number).! ~ ", " ~
        "z=" ~ ("-".? ~ number).! ~ ">" ~ End)
      .map { case (x, y, z) =>
        Vector3(x.toInt, y.toInt, z.toInt) }

    parse(rawCoordinate, coordinate(_)) match {
      case Parsed.Success(p, _) => p
      case _ => throw new RuntimeException(s"Failed to parse: $rawCoordinate")
    }
  }

  def applyGravity(moons: Seq[Moon]): Seq[Moon] = {
    moons.map { moon1 =>
      moons.foldLeft(moon1) { (moon, moon2) =>
        val delta = moon2.diff(moon)
        moon.copy(v = moon.v + delta)
      }
    }
  }

  def applyVelocity(moons: Seq[Moon]): Seq[Moon] = {
    moons.map { moon => moon.copy(p = moon.p + moon.v) }
  }
}
