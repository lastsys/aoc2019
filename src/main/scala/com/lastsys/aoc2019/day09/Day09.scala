package com.lastsys.aoc2019.day09

import com.lastsys.aoc2019.util.{AocTask, IntCode, loadResource}

object Day09 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day09.txt")

    input.foreach { data =>
      val program = data.head.split(",")

      {
        var vm = IntCode(IntCode.convertFromString(program)).putInput(1)
        while (!vm.halt) {
          vm = vm.step
        }
        val part1 = vm.output.head
        println(s"Day09 :: Part1 = $part1")
      }

      {
        var vm = IntCode(IntCode.convertFromString(program)).putInput(2)
        while (!vm.halt) {
          vm = vm.step
        }
        val part2 = vm.output.head
        println(s"Day09 :: Part2 = $part2")
      }
    }
  }
}
