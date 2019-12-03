package com.lastsys.aoc2019.day03

import com.lastsys.aoc2019.util.AocTask
import com.lastsys.aoc2019.util.loadResource
import com.lastsys.aoc2019.util.manhattanDistance

import scala.collection.mutable

sealed trait Direction
case class Up(distance: Int) extends Direction
case class Right(distance: Int) extends Direction
case class Left(distance: Int) extends Direction
case class Down(distance: Int) extends Direction

case class Instruction(direction: Direction, distance: Int)

object Day03 extends AocTask {
  type Point = (Int, Int)
  type PathMap = Map[Point, Int]

  override def run(): Unit = {
    val input = loadResource("day03.txt")

    input.foreach { data =>
      val path1 = parsePath(data.head)
      val path2 = parsePath(data(1))

      val part1 = distanceToClosestIntersection(path1, path2)
      println(s"Day03 :: Part 1 = $part1")

      val part2 = closestIntersectionBySteps(path1, path2)
      println(s"Day03 :: Part 2 = $part2")
    }
  }

  /** Calculate distance to closest intersection.
   *
   * @param path1 first path
   * @param path2 second path
   * @return distance to closest intersection
   */
  def distanceToClosestIntersection(path1: Seq[Direction], path2: Seq[Direction]): Int = {
    val map1 = generateMap(path1)
    val map2 = generateMap(path2)
    findIntersections(map1, map2).map(p => manhattanDistance((0, 0), p)).min
  }

  /** Calculate closest intersection by steps in second path.
   *
   * @param path1 first path
   * @param path2 second path
   * @return steps to the closest intersection
   */
  def closestIntersectionBySteps(path1: Seq[Direction], path2: Seq[Direction]): Int = {
    val map1 = generateMap(path1)
    val map2 = generateMap(path2)
    findIntersections(map1, map2)
      .foldLeft(Seq.empty[Int])((acc, value) => acc :+ map1(value) + map2(value))
      .min
  }

  /** Parse string of path description to sequence of directions.
   *
   * @param path string representation of path
   * @return sequence of directions
   */
  def parsePath(path: String): Seq[Direction] =
    path.split(",").map {
      case s if s.startsWith("U") => Up(s.drop(1).toInt)
      case s if s.startsWith("R") => Right(s.drop(1).toInt)
      case s if s.startsWith("D") => Down(s.drop(1).toInt)
      case s if s.startsWith("L") => Left(s.drop(1).toInt)
    }

  /** Find intersections between two maps.
   *
   * @param map1 first map
   * @param map2 second map
   * @return set of intersections
   */
  def findIntersections(map1: PathMap, map2: PathMap): Set[Point] =
    map1.keys.toSet.intersect(map2.keys.toSet)

  /** Generate a set of coordinates for a sequence of directions.
   *
   * @param path sequence of directions
   * @return set of coordinates
   */
  def generateMap(path: Seq[Direction]): PathMap = {
    val m = mutable.Map[(Int, Int), Int]()
    var pos: Point = (0, 0)
    var totalSteps = 0

    path.foreach {
      case Up(steps) =>
        (pos._2 + 1).to(pos._2 + steps).foreach { y =>
          pos = (pos._1, y)
          totalSteps += 1
          if (!m.contains(pos)) {
            m(pos) = totalSteps
          }
        }
      case Down(steps) =>
        (pos._2 - 1).to(pos._2 - steps, -1).foreach { y =>
          pos = (pos._1, y)
          totalSteps += 1
          if (!m.contains(pos)) {
            m(pos) = totalSteps
          }
        }
      case Right(steps) =>
        (pos._1 + 1).to(pos._1 + steps).foreach { x =>
          pos = (x, pos._2)
          totalSteps += 1
          if (!m.contains(pos)) {
            m(pos) = totalSteps
          }
        }
      case Left(steps) =>
        (pos._1 - 1).to(pos._1 - steps, -1).foreach { x =>
          pos = (x, pos._2)
          totalSteps += 1
          if (!m.contains(pos)) {
            m(pos) = totalSteps
          }
        }
    }
    m.toMap
  }
}