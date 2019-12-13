package com.lastsys.aoc2019.day11

import com.lastsys.aoc2019.util.{AocTask, IntCode, loadResource}

import scala.collection.mutable
import scala.util.chaining._

case class Point(x: Int, y: Int) {
  def +(p2: Point) = Point(x + p2.x, y + p2.y)
}

case class Turtle(p: Point, dx: Int, dy: Int) {
  def turnLeft: Turtle = Turtle(p, dy, -dx)

  def turnRight: Turtle = Turtle(p, -dy, dx)

  def moveForward: Turtle = Turtle(p + Point(dx, dy), dx, dy)
}

object Day11 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day11.txt")

    input.foreach { data =>
      val program = IntCode.convertFromString(data.head.split(","))

      val part1 = walk(program)
      println(s"Day11 :: Part1 = ${part1.size}")
    }
  }

  def walk(program: Map[BigInt, BigInt]): Set[Point] = {
    val hull = mutable.Set.empty[Point]
    var turtle = Turtle(Point(0, 0), 0, -1)

    var intCode = IntCode(program)
    intCode = intCode.putInput(0)

    while (!intCode.halt) {
      printHull(hull.toSet, turtle)
      println()
      val (ic2, color) = intCode.getOutput
      intCode = ic2
      if (!intCode.halt) {
        val (ic3, direction) = ic2.getOutput
        intCode = ic3
        if (!intCode.halt) {
          if (color == 1) hull += turtle.p else hull -= turtle.p
          turtle = direction.toInt match {
            case 0 => turtle.turnLeft.moveForward
            case 1 => turtle.turnRight.moveForward
          }
          intCode = intCode.putInput(if (hull.contains(turtle.p)) 1 else 0)
        }
      }
    }
    hull.toSet
  }

  def printHull(hull: Set[Point], turtle: Turtle): Unit = {
    for (y <- -20 to 20) {
      for (x <- -40 to 40) {
        val p = Point(x, y)
        if (turtle.p == p) {
          (turtle.dx, turtle.dy) match {
            case (0, -1) => print("^")
            case (0, 1) => print("v")
            case (-1, 0) => print("<")
            case (1, 0) => print(">")
            case _ => println(s"\nGot $turtle")
          }
        } else {
          if (hull.contains(p)) print("#") else print(".")
        }
      }
      println()
    }
  }

  println("\n")
}
