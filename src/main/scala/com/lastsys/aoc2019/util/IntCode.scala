package com.lastsys.aoc2019.util

import scala.collection.immutable.Queue

case class IntCode(mem: Map[Long, Long],
                   pc: Long = 0,
                   relativeBase: Long = 0,
                   input: Queue[Long] = Queue.empty,
                   output: Queue[Long] = Queue.empty,
                   waitingForInput: Boolean = false,
                   halt: Boolean = false) {

  def putInput(value: Long): IntCode = this.copy(input = input.enqueue(value), waitingForInput = false)

  def getOutput: (IntCode, Option[Long]) = {
    var m = this
    if (m.output.isEmpty)  {
      while (m.output.isEmpty && !m.halt && !m.waitingForInput) {
        m = m.step
      }
    }
    if (!m.halt && m.output.nonEmpty) {
      val (value, queue) = m.output.dequeue
      (m.copy(output = queue), Some(value))
    } else {
      (m, None)
    }
  }

  def step: IntCode = {
    val instruction: Long = mem.getOrElse(pc, 0)

    val m3 = instruction / 10000
    val m2 = (instruction - m3 * 10000) / 1000
    val m1 = (instruction - m3 * 10000 - m2 * 1000) / 100
    val opCode = (instruction - m3 * 10000 - m2 * 1000 - m1 * 100).toInt

    if (opCode == 99 || halt) {
      this.copy(halt = true)
    } else opCode match {
      // Addition
      case 1 =>
        val arg1 = read(m1, mem(pc + 1))
        val arg2 = read(m2, mem(pc + 2))
        val arg3 = readLiteral(m3, mem(pc + 3))
        this.copy(mem.updated(arg3, arg1 + arg2), pc = pc + 4)
      // Multiplication
      case 2 =>
        val arg1 = read(m1, mem(pc + 1))
        val arg2 = read(m2, mem(pc + 2))
        val arg3 = readLiteral(m3, mem(pc + 3))
        this.copy(mem.updated(arg3, arg1 * arg2), pc = pc + 4)
      // Input
      case 3 =>
        val arg1 = readLiteral(m1, mem(pc + 1))
        if (input.nonEmpty) {
          this.copy(mem.updated(arg1, input.head), input = input.tail, pc = pc + 2, waitingForInput = false)
        } else {
          this.copy(waitingForInput = true)
        }
      // Output
      case 4 =>
        val arg1 = read(m1, mem(pc + 1))
        this.copy(output = output.enqueue(arg1), pc = pc + 2)
      // Jump if true
      case 5 =>
        val arg1 = read(m1, mem(pc + 1))
        val arg2 = read(m2, mem(pc + 2))
        this.copy(pc = if (arg1 != 0) arg2 else pc + 3)
      // Jump if false
      case 6 =>
        val arg1 = read(m1, mem(pc + 1))
        val arg2 = read(m2, mem(pc + 2))
        this.copy(pc = if (arg1 == 0) arg2 else pc + 3)
      // Less than
      case 7 =>
        val arg1 = read(m1, mem(pc + 1))
        val arg2 = read(m2, mem(pc + 2))
        val arg3 = readLiteral(m3, mem(pc + 3))
        this.copy(mem.updated(arg3, if (arg1 < arg2) 1 else 0), pc = pc + 4)
      // Equals
      case 8 =>
        val arg1 = read(m1, mem(pc + 1))
        val arg2 = read(m2, mem(pc + 2))
        val arg3 = readLiteral(m3, mem(pc + 3))
        this.copy(mem.updated(arg3, if (arg1 == arg2) 1 else 0), pc = pc + 4)
      // Adjust relative base
      case 9 =>
        val arg1 = read(m1, mem(pc + 1))
        this.copy(relativeBase = relativeBase + arg1, pc = pc + 2)
    }
  }

  def read(mode: Long, value: Long): Long =
    mode.toInt match {
      // position mode
      case 0 => mem.getOrElse(value, 0)
      // immediate mode
      case 1 => value
      // relative mode
      case 2 => mem.getOrElse(relativeBase + value, 0)
    }

  def readLiteral(mode: Long, value: Long): Long =
    mode.toInt match {
      case 0 => value
      case 1 => value
      case 2 => relativeBase + value
    }
}

object IntCode {
  def convertFromString(program: Seq[String]): Map[Long, Long] = {
    program.zipWithIndex.foldLeft(Map.empty[Long, Long]) { case (acc, (v, i)) =>
      // TODO: Make sure this works with Long later on.
      acc + (i.toLong -> v.toLong)
    }
  }

  def convertFromInt(program: Seq[Int]): Map[Long, Long] = {
    program.zipWithIndex.foldLeft(Map.empty[Long, Long]) { case (acc, (v, i)) =>
      // TODO: Make sure this works with Long later on.
      acc + (i.toLong -> v.toLong)
    }
  }
}
