package com.lastsys.aoc2019.day13

import com.lastsys.aoc2019.util.{AocTask, IntCode, loadResource}

import scala.collection.mutable

sealed trait Tile {
  val char: String
  val number: Long
}

case object Empty extends Tile {
  val char = "."
  val number = 0
}

case object Wall extends Tile {
  val char = "#"
  val number = 1
}

case object Block extends Tile {
  val char = "X"
  val number = 2
}

case object Paddle extends Tile {
  val char = "@"
  val number = 3
}

case object Ball extends Tile {
  val char = "O"
  val number = 4
}

sealed trait Mode

case object Init extends Mode

case object Play extends Mode

object Day13 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day13.txt")

    input.foreach { data =>
      val program = IntCode.convertFromString(data.head.split(","))

      val m = readMap(program)
      println(s"Day13 :: Part1 = ${countBlocks(m)}")

      val score = readMap(program)
      println(s"Day13 :: Part2 = ${play(program)}")
    }
  }

  def countBlocks(m: Map[(Int, Int), Tile]): Int =
    m.values.count(_ == Block)

  def readMap(program: Map[Long, Long]): Map[(Int, Int), Tile] = {
    var vm = IntCode(program)
    val tiles = mutable.Map.empty[(Int, Int), Tile]
    while (!vm.halt) {
      val (vm1, x) = vm.getOutput
      val (vm2, y) = vm1.getOutput
      val (vm3, t) = vm2.getOutput
      (x, y, t) match {
        case (Some(x), Some(y), Some(t)) =>
          val tile = num2tile(t)
          tiles += ((x.toInt, y.toInt) -> tile)
        case _ =>
      }
      vm = vm3
    }
    tiles.toMap
  }

  def printMap(m: Map[(Int, Int), Tile]): Unit = {
    val (minX, maxX, minY, maxY) =
      m.keys.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)) {
        case ((mnx, mxx, mny, mxy), coord) =>
          (Math.min(mnx, coord._1),
            Math.max(mxx, coord._1),
            Math.min(mny, coord._2),
            Math.max(mxy, coord._2))
      }

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        if (m.contains(x, y)) {
          print(m((x, y)).char)
        } else {
          print(" ")
        }
      }
      println()
    }
  }

  def num2tile(t: Long): Tile =
    t match {
      case Empty.number => Empty
      case Wall.number => Wall
      case Block.number => Block
      case Paddle.number => Paddle
      case Ball.number => Ball
    }

  def play(program: Map[Long, Long]): Int = {
    var score = 0
    // Initialize vm and insert quarters. 2 = play for free
    //    var tiles = readMap(program)
    var tiles = Map.empty[(Int, Int), Tile]
    var vm = IntCode(program + (0L -> 2L))
    var paddleX: Option[Int] = None
    var ballX: Option[Int] = None
    while (!vm.halt) {
//      if (paddleX.nonEmpty && ballX.nonEmpty) {
//        println(s"Score: $score")
//        printMap(tiles)
//      }

      val (vm1, x) = vm.getOutput
      val (vm2, y) = vm1.getOutput
      val (vm3, t) = vm2.getOutput
      vm = vm3
      (x, y, t) match {
        case (Some(-1), Some(0), Some(s)) =>
          score = s.toInt
        case (Some(x), Some(y), Some(Paddle.number)) =>
          paddleX = Some(x.toInt)
          tiles = tiles + ((x.toInt, y.toInt) -> Paddle)
        case (Some(x), Some(y), Some(Ball.number)) =>
          ballX = Some(x.toInt)
          tiles = tiles + ((x.toInt, y.toInt) -> Ball)
        case (Some(x), Some(y), Some(t)) =>
          val tile = num2tile(t)
          tiles = tiles + ((x.toInt, y.toInt) -> tile)
        case _ if vm.waitingForInput =>
          vm = (paddleX, ballX) match {
            case (Some(px), Some(bx)) =>
              if (px < bx) {
                vm.putInput(1L)
              } else if (px > bx) {
                vm.putInput(-1L)
              } else {
                vm.putInput(0)
              }
            case _ => vm
          }
        case _ => vm = vm.copy(halt = true)
      }
    }
    score
  }

  def ballX(m: Map[(Int, Int), Tile]): Option[Int] = {
    val b = m.filter {
      case (_, t) => t == Ball
    }
    b.keys.headOption.map(_._1)
  }

  def paddleX(m: Map[(Int, Int), Tile]): Option[Int] = {
    val p = m.filter {
      case (_, t) => t == Paddle
    }
    p.keys.headOption.map(_._1)
  }
}
