package com.lastsys.aoc2019.day07

import com.lastsys.aoc2019.util.{AocTask, loadResource}

import scala.annotation.tailrec

object Day07 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day07.txt")

    input.foreach { data =>
      val program = data.head.split(",").map(_.toInt)

      val part1 = maximizeThruster(program)._1
      println(s"Day07 :: Part1 = $part1")
    }
  }

  def maximizeThruster(program: Seq[Int], low: Int = 0, high: Int = 4): (Int, Seq[Int]) = {
    var maxThrust = Int.MinValue
    var sequence = Seq.empty[Int]
    for (a <- low to high;
         b <- low to high if b != a;
         c <- low to high if c != b && c != a;
         d <- low to high if d != c && d != b && d != a;
         e <- low to high if e != d && e != d && e != c && e != b && e != a) {

      val s = Seq(a, b, c, d, e)
      val t = thrust(program, Seq(a, b, c, d, e))
      if (t > maxThrust) {
        maxThrust = t
        sequence = s
      }
    }
    (maxThrust, sequence)
  }

  def thrust(program: Seq[Int], phase: Seq[Int]): Int =
    phase.foldLeft(0) { (input, phase) =>
      execute(program, Seq(phase, input))._2.toInt
    }

  @tailrec
  def execute(program: Seq[Int], input: Seq[Int], pc: Int = 0, output: String = ""): (Seq[Int], String) = {
    val instruction = program(pc)

    def read(mode: Int, value: Int): Int =
      if (mode == 0) { // position mode
        program(value)
      } else {         // immediate mode
        value
      }

    val m3 = instruction / 10000
    val m2 = (instruction - m3 * 10000) / 1000
    val m1 = (instruction - m3 * 10000 - m2 * 1000) / 100
    val opcode = instruction - m3 * 10000 - m2 * 1000 - m1 * 100

    if (opcode == 99) {
      (program, output)
    } else {
      val arg1 = read(m1, program(pc + 1))

      opcode match {
        // Addition
        case 1 =>
          val arg2 = read(m2, program(pc + 2))
          val arg3 = program(pc + 3)
          execute(program.updated(arg3, arg1 + arg2), input, pc + 4, output)
        // Multiplication
        case 2 =>
          val arg2 = read(m2, program(pc + 2))
          val arg3 = program(pc + 3)
          execute(program.updated(arg3, arg1 * arg2), input, pc + 4, output)
        // Input
        case 3 =>
          val arg1 = program(pc + 1)
          execute(program.updated(arg1, input.head), input.tail, pc + 2, output)
        // Output
        case 4 =>
          execute(program, input, pc + 2, output + arg1)
        // Jump if true
        case 5 =>
          val arg2 = read(m2, program(pc + 2))
          execute(program, input, if (arg1 != 0) arg2 else pc + 3, output)
        // Jump if false
        case 6 =>
          val arg2 = read(m2, program(pc + 2))
          execute(program, input, if (arg1 == 0) arg2 else pc + 3, output)
        // Less than
        case 7 =>
          val arg2 = read(m2, program(pc + 2))
          val arg3 = program(pc + 3)
          execute(program.updated(arg3, if (arg1 < arg2) 1 else 0), input, pc + 4, output)
        // Equals
        case 8 =>
          val arg2 = read(m2, program(pc + 2))
          val arg3 = program(pc + 3)
          execute(program.updated(arg3, if (arg1 == arg2) 1 else 0), input, pc + 4, output)
      }
    }
  }
}
