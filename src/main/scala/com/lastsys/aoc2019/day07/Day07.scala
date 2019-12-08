package com.lastsys.aoc2019.day07

import com.lastsys.aoc2019.util.{AocTask, loadResource}

import scala.annotation.tailrec
import scala.collection.mutable
import util.control.Breaks._

case class ExecutionStatus(program: Seq[Int], output: String, pc: Int, inputs: Seq[Int], halt: Boolean = false)

object Day07 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day07.txt")

    input.foreach { data =>
      val program = data.head.split(",").map(_.toInt)

      val part1 = maximizeThruster(program)._1
      println(s"Day07 :: Part1 = $part1")

      val part2 = maximizeThruster(program, low = 5, high = 9, feedback = true)._1
      println(s"Day07 :: Part2 = $part2")
    }
  }

  def maximizeThruster(program: Seq[Int], low: Int = 0, high: Int = 4, feedback: Boolean = false): (Int, Seq[Int]) = {
    var maxThrust = Int.MinValue
    var sequence = Seq.empty[Int]
    for (a <- low to high;
         b <- low to high if b != a;
         c <- low to high if c != b && c != a;
         d <- low to high if d != c && d != b && d != a;
         e <- low to high if e != d && e != d && e != c && e != b && e != a) {

      val s = Seq(a, b, c, d, e)
      val t = if (!feedback) {
        thrust(program, s)
      } else {
        thrustWithFeedback(program, s)
      }
      if (t > maxThrust) {
        maxThrust = t
        sequence = s
      }
    }
    (maxThrust, sequence)
  }

  def thrust(program: Seq[Int], phase: Seq[Int]): Int =
    phase.foldLeft(0) { (input, phase) =>
      execute(program, Seq(phase, input)).output.toInt
    }

  def thrustWithFeedback(program: Seq[Int], phase: Seq[Int]): Int = {
    val programs = mutable.ArrayBuffer(program, program, program, program, program)
    val pcs = mutable.ArrayBuffer(0, 0, 0, 0, 0)
    val inputs = mutable.ArrayBuffer(Seq(phase.head), Seq(phase(1)), Seq(phase(2)), Seq(phase(3)), Seq(phase(4)))
    val queue = mutable.Queue(0)
    var halt = false
    var thruster = 0

    breakable {
      while (!halt) {
        for (i <- 0 to 4) {
          val input: Seq[Int] = if (queue.isEmpty) inputs(i) else inputs(i) :+ queue.dequeue()
          val result = execute(programs(i), input, pcs(i), pausable = true)
          halt = result.halt
          if (halt) break
          if (i == 4) thruster = result.output.toInt
          programs(i) = result.program
          pcs(i) = result.pc
          inputs(i) = result.inputs
          queue.enqueue(result.output.toInt)
        }
      }
    }

    thruster
  }

  @tailrec
  def execute(program: Seq[Int], input: Seq[Int], pc: Int = 0,
              output: String = "",
              pausable: Boolean = false, pause: Boolean = false): ExecutionStatus = {

    if (pause) {
      ExecutionStatus(program, output, pc, input)
    } else {

      val instruction = program(pc)

      def read(mode: Int, value: Int): Int =
        if (mode == 0) { // position mode
          program(value)
        } else { // immediate mode
          value
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
          case 1 =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = program(pc + 3)
            execute(program.updated(arg3, arg1 + arg2), input, pc + 4, output, pausable = pausable)
          // Multiplication
          case 2 =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = program(pc + 3)
            execute(program.updated(arg3, arg1 * arg2), input, pc + 4, output, pausable = pausable)
          // Input
          case 3 =>
            val arg1 = program(pc + 1)
            execute(program.updated(arg1, input.head), input.tail, pc + 2, output, pausable = pausable)
          // Output
          case 4 =>
            execute(program, input, pc + 2, output + arg1, pausable = pausable, pause = pausable)
          // Jump if true
          case 5 =>
            val arg2 = read(m2, program(pc + 2))
            execute(program, input, if (arg1 != 0) arg2 else pc + 3, output, pausable = pausable)
          // Jump if false
          case 6 =>
            val arg2 = read(m2, program(pc + 2))
            execute(program, input, if (arg1 == 0) arg2 else pc + 3, output, pausable = pausable)
          // Less than
          case 7 =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = program(pc + 3)
            execute(program.updated(arg3, if (arg1 < arg2) 1 else 0), input, pc + 4, output, pausable = pausable)
          // Equals
          case 8 =>
            val arg2 = read(m2, program(pc + 2))
            val arg3 = program(pc + 3)
            execute(program.updated(arg3, if (arg1 == arg2) 1 else 0), input, pc + 4, output, pausable = pausable)
        }
      }
    }
  }
}
