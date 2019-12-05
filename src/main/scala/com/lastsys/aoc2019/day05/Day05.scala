package com.lastsys.aoc2019.day05

import com.lastsys.aoc2019.util.{AocTask, loadResource}

import scala.annotation.tailrec

object Day05 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day05.txt")

    input.foreach { data =>
      val program = data.head.split(",").map(_.toInt)

      val (_, part1) = execute(program)(1)
      println(s"Day05 :: Part1 = $part1")

      val (_, part2) = execute(program)(5)
      println(s"Day05 :: Part2 = $part2")
    }
  }

  @tailrec
  def execute(program: Seq[Int], pc: Int = 0, output: String = "")(input: Int = 1): (Seq[Int], String) = {
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
          execute(program.updated(arg3, arg1 + arg2), pc + 4, output)(input)
          // Multiplication
        case 2 =>
          val arg2 = read(m2, program(pc + 2))
          val arg3 = program(pc + 3)
          execute(program.updated(arg3, arg1 * arg2), pc + 4, output)(input)
          // Input
        case 3 =>
          val arg1 = program(pc + 1)
          execute(program.updated(arg1, input), pc + 2, output)(input)
          // Output
        case 4 =>
          execute(program, pc + 2, output + arg1)(input)
          // Jump if true
        case 5 =>
          val arg2 = read(m2, program(pc + 2))
          execute(program, if (arg1 != 0) arg2 else pc + 3, output)(input)
          // Jump if false
        case 6 =>
          val arg2 = read(m2, program(pc + 2))
          execute(program, if (arg1 == 0) arg2 else pc + 3, output)(input)
          // Less than
        case 7 =>
          val arg2 = read(m2, program(pc + 2))
          val arg3 = program(pc + 3)
          execute(program.updated(arg3, if (arg1 < arg2) 1 else 0), pc + 4, output)(input)
          // Equals
        case 8 =>
          val arg2 = read(m2, program(pc + 2))
          val arg3 = program(pc + 3)
          execute(program.updated(arg3, if (arg1 == arg2) 1 else 0), pc + 4, output)(input)
      }
    }
  }
}
