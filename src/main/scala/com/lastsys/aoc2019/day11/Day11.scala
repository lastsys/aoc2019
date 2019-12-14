package com.lastsys.aoc2019.day11

import com.lastsys.aoc2019.util.{AocTask, IntCode, loadResource}

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
      val program = IntCode.convertFromString(data.head.split(","))

      val part1 = walk(program, 0)
      println(s"Day11 :: Part1 = ${part1._1.size}")

      val part2 = walk(program, 1)
      println(s"Day11 :: Part2 =")
      printHull(part2._2, None)
    }
  }

  def walk(program: Map[Long, Long], firstInput: Int): (Set[Point], Set[Point]) = {
    val hull = mutable.Set.empty[Point]
    val painted = mutable.Set.empty[Point]
    var turtle = Turtle(Point(0, 0), 0, -1)

    var vm = IntCode(program).putInput(firstInput)

    while (!vm.halt) {
      val (ic2, color) = vm.getOutput
      vm = ic2
      val (ic3, direction) = vm.getOutput
      vm = ic3
      if (color == 1) hull += turtle.p else hull -= turtle.p
      painted += turtle.p
      turtle = direction.toInt match {
        case 0 => turtle.turnLeft.moveForward
        case 1 => turtle.turnRight.moveForward
      }
      vm = vm.putInput(if (hull.contains(turtle.p)) 1 else 0)
    }
    (painted.toSet, hull.toSet)
  }

  def printHull(hull: Set[Point], turtle: Option[Turtle]): Unit = {
    val (minX, maxX, minY, maxY) = hull.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)) {
      case ((minX, maxX, minY, maxY), p) => (Math.min(minX, p.x), Math.max(maxX, p.x),
        Math.min(minY, p.y), Math.max(maxY, p.y))
    }

    for (y <- minY - 5 to maxY + 5) {
      for (x <- minX - 5 to maxX + 5) {
        val p = Point(x, y)
        turtle match {
          case Some(t) if t.p == p =>
            (t.dx, t.dy) match {
              case (0, -1) => print("^")
              case (0, 1) => print("v")
              case (-1, 0) => print("<")
              case (1, 0) => print(">")
              case _ => println(s"\nGot $turtle")
            }
          case _ =>
            if (hull.contains(p)) print("#") else print(".")
        }
      }
      println()
    }
    println("\n")
  }
}
