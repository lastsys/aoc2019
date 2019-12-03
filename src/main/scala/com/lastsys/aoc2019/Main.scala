package com.lastsys.aoc2019

import com.lastsys.aoc2019.day01.Day01
import com.lastsys.aoc2019.day02.Day02
import com.lastsys.aoc2019.day03.Day03
import com.lastsys.aoc2019.util.time

object Main {
  def main(args: Array[String]): Unit = {
    time { Day01.run() }
    time { Day02.run() }
    time { Day03.run() }
  }
}
