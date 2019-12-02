package com.lastsys.aoc2019.day02

import com.lastsys.aoc2019.util.AocTask
import com.lastsys.aoc2019.util.loadResource

import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day02 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day02.txt")

    input.foreach { data =>
      val program = data.head.split(",").map(_.toInt)
      val adjustedProgram = program.updated(1, 12).updated(2, 2)

      val part1 = execute(adjustedProgram).head
      println(s"Day02 :: Part 1 = $part1")

      breakable {
        for (noun <- 0 to 99) {
          for (verb <- 0 to 99) {
            val newProgram = program.updated(1, noun).updated(2, verb)
            if (execute(newProgram).head == 19690720) {
              println(s"Day02 :: Part 2 = ${100 * noun + verb}")
              break
            }
          }
        }
      }
    }
  }

  @tailrec
  def execute(program: Seq[Int], pc: Int = 0): Seq[Int] = {
    val opCode = program(pc)
    // End program.
    if (opCode == 99) {
      program
    } else {
      val input1 = program(program(pc + 1))
      val input2 = program(program(pc + 2))
      val output = program(pc + 3)
      opCode match {
        // Addition
        case 1 =>
          execute(program.updated(output, input1 + input2), pc + 4)
        // Multiplication
        case 2 =>
          execute(program.updated(output, input1 * input2), pc + 4)
      }
    }
  }
}
