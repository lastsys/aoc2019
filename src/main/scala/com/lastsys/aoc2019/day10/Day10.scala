package com.lastsys.aoc2019.day10

import com.lastsys.aoc2019.util.{AocTask, loadResource}

import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def -(p2: Point): Point = Point(x - p2.x, y - p2.y)
}

object Day10 extends AocTask {

  override def run(): Unit = {
    val input = loadResource("day10.txt")

    input.foreach { data =>
      val m = parseMap(data)
      val (_, count) = findBestAsteroid(m)
      println(s"Day10 :: Part1 = $count")
    }
  }

  def findBestAsteroid(m: Set[Point]): (Point, Int) =
    m.foldLeft((Point(0, 0), 0)) { case ((best, count), p) =>
      val thisCount = countVisible(m, p)
      if (thisCount > count) (p, thisCount) else (best, count)
    }

  def countVisible(m: Set[Point], p1: Point): Int = {
    val counts = (m - p1).map { p2 => p2 -> isVisible(m, p1, p2) }.toMap
    counts.values.count(identity)
  }

  def isVisible(m: Set[Point], p1: Point, p2: Point): Boolean =
    (m - p1 - p2).foldLeft(true) { (visible, asteroid) =>
      // Based on cross product. Zero if co-linear, thus blocked.
      val dp1 = asteroid - p1
      val dp2 = p2 - p1
      val isColinear = (dp1.x * dp2.y - dp1.y * dp2.x) == 0
      val inInterval = Math.max(p1.x, p2.x) >= asteroid.x && Math.min(p1.x, p2.x) <= asteroid.x &&
        Math.max(p1.y, p2.y) >= asteroid.y && Math.min(p1.y, p2.y) <= asteroid.y
      if (isColinear && inInterval) false else visible
    }

  def parseMap(raw: Seq[String]): Set[Point] =
    raw.zipWithIndex.foldLeft(Set.empty[Point]) { case (acc, (row, y)) =>
      row.zipWithIndex.foldLeft(acc) { case (acc, (char, x)) =>
        char match {
          case '#' => acc + Point(x, y)
          case '.' => acc
        }
      }
    }

  def findBestLocation(m: Set[Point]): (Point, Seq[Point]) =
    m.foldLeft((Point(0, 0), Seq.empty[Point])) { case ((best, order), p) =>
      val testOrder = eliminateInOrder(m, p)
      if (testOrder.length > order.length) (p, testOrder) else (best, order)
    }

  def eliminateInOrder(m: Set[Point], p: Point): Seq[Point] = {

    def eliminate(m: Set[Point]): Seq[Point] = {
      if (m.isEmpty) Seq.empty else {
        val visible = findVisible(m, p)
        val sorted = sortPointsByAngle(visible, p)
        sorted ++ eliminate(m.union(sorted.toSet))
      }
    }

    eliminate(m - p)
  }

  def findVisible(m: Set[Point], p: Point): Set[Point] = {
    val other = m - p
    other.foldLeft(Set.empty[Point]) { (found, asteroid) =>
      if (isVisible(other - asteroid, p, asteroid)) found + asteroid else found
    }
  }

  def sortPointsByAngle(p: Set[Point], origin: Point): Seq[Point] = {
    p.toSeq.sortWith(cmpAngle(origin))
  }

  def cmpAngle(origin: Point)(p1: Point, p2: Point): Boolean = {
    val dp1 = p1 - origin
    val dp2 = p2 - origin
    val a1 = Math.atan2(dp1.y, dp1.x)
    val a2 = Math.atan2(dp2.y, dp2.x)
    (a2 - a1) < 0
  }
}
