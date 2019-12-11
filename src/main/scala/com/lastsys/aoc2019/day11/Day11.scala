package com.lastsys.aoc2019.day11

import com.lastsys.aoc2019.util.{AocTask, Big, ExecutionStatus, IntCode, loadResource}

import scala.collection.mutable

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
      val program = IntCode.convertProgramToMapMem(data.head.split(","))

      val part1 = walk(program)
      println(s"Day11 :: Part1 = ${part1.size}")
    }
  }

  def walk(program: Map[BigInt, BigInt]): Set[Point] = {
    val hull = mutable.Set.empty[Point]
    var turtle = Turtle(Point(0, 0), 0, -1)
    var halt = false
    var colorMode = true
    var status = ExecutionStatus(program)

    while (!halt) {
      status = IntCode.execute(status.program,
        Seq(if (hull.contains(turtle.p)) 1 else 0), output = status.output, pausable = true)
      halt = status.halt
      if (!halt) {
        if (colorMode) {
          if (status.output.head == 1) hull += turtle.p
        } else {
          turtle = status.output.head match {
            // Turn left.
            case Big(0) => turtle.turnLeft.moveForward
            // Turn right.
            case Big(1) => turtle.turnRight.moveForward
          }
        }
      }
      colorMode = !colorMode
    }

    hull.toSet
  }
}
