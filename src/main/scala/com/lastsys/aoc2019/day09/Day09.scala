package com.lastsys.aoc2019.day09

import com.lastsys.aoc2019.util.{AocTask, loadResource}

import scala.annotation.tailrec

case class ExecutionStatus(program: Map[BigInt, BigInt], output: String, pc: BigInt, relativeBase: BigInt, halt: Boolean = false)

object Big {
  def unapply(n: BigInt) = Some(n.toInt)
}

object Day09 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day09.txt")

    input.foreach { data =>
      val program = data.head.split(",")

      val part1 = execute(convertProgramToMapMem(program), 1)
      println(s"Day09 :: Part1 = ${part1.output}")

      val part2 = execute(convertProgramToMapMem(program), 2)
      println(s"Day09 :: Part2 = ${part2.output}")
    }
  }

  def convertProgramToMapMem(program: Seq[String]): Map[BigInt, BigInt] =
    program.zipWithIndex.foldLeft(Map.empty[BigInt, BigInt]) { case (acc, (v, i)) =>
      acc + (BigInt(i) -> BigInt(v))
    }

  @tailrec
  def execute(program: Map[BigInt, BigInt], input: BigInt, pc: BigInt = 0, relativeBase: BigInt = 0,
              output: String = "",
              pausable: Boolean = false, pause: Boolean = false): ExecutionStatus = {

    if (pause) {
      ExecutionStatus(program, output, pc, relativeBase)
    } else {

      val instruction: BigInt = program.getOrElse(pc, 0)

      def read(mode: BigInt, value: BigInt): BigInt =
        mode match {
          // position mode
          case Big(0) => program.getOrElse(value, 0)
          // immediate mode
          case Big(1) => value
          // relative mode
          case Big(2) => program.getOrElse(relativeBase + value, 0)
        }

      def readLiteral(mode: BigInt, value: BigInt): BigInt =
        mode match {
          case Big(0) => value
          case Big(1) => value
          case Big(2) => relativeBase + value
        }

      val m3 = instruction / 10000
      val m2 = (instruction - m3 * 10000) / 1000
      val m1 = (instruction - m3 * 10000 - m2 * 1000) / 100
      val opcode = instruction - m3 * 10000 - m2 * 1000 - m1 * 100

      if (opcode == 99) {
        ExecutionStatus(program, output, pc, input, halt = true)
      } else {
        val arg1 = read(m1, program(pc + 1))

        opcode match {
          // Addition
          case Big(1) =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = readLiteral(m3, program(pc + 3))
            execute(program.updated(arg3, arg1 + arg2), input, pc + 4, relativeBase, output, pausable = pausable)
          // Multiplication
          case Big(2) =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = readLiteral(m3, program(pc + 3))
            execute(program.updated(arg3, arg1 * arg2), input, pc + 4, relativeBase, output, pausable = pausable)
          // Input
          case Big(3) =>
            val arg1 = readLiteral(m1, program(pc + 1))
            execute(program.updated(arg1, input), input, pc + 2, relativeBase, output, pausable = pausable)
          // Output
          case Big(4) =>
            execute(program, input, pc + 2, relativeBase, output + arg1, pausable = pausable, pause = pausable)
          // Jump if true
          case Big(5) =>
            val arg2 = read(m2, program(pc + 2))
            execute(program, input, if (arg1 != 0) arg2 else pc + 3, relativeBase, output, pausable = pausable)
          // Jump if false
          case Big(6) =>
            val arg2 = read(m2, program(pc + 2))
            execute(program, input, if (arg1 == 0) arg2 else pc + 3, relativeBase, output, pausable = pausable)
          // Less than
          case Big(7) =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = readLiteral(m3, program(pc + 3))
            execute(program.updated(arg3, if (arg1 < arg2) 1 else 0), input, pc + 4, relativeBase, output, pausable = pausable)
          // Equals
          case Big(8) =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = readLiteral(m3, program(pc + 3))
            execute(program.updated(arg3, if (arg1 == arg2) 1 else 0), input, pc + 4, relativeBase, output, pausable = pausable)
          // Adjust relative base
          case Big(9) =>
            execute(program, input, pc + 2, relativeBase + arg1, output, pausable = pausable)
        }
      }
    }
  }
}
