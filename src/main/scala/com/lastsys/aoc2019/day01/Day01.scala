package com.lastsys.aoc2019.day01

import com.lastsys.aoc2019.util.AocTask
import com.lastsys.aoc2019.util.loadResource

import scala.annotation.tailrec

object Day01 extends AocTask {
  override def run(): Unit = {
    val input = loadResource("day01.txt")

    input.foreach { data =>
      val part1 = data.map(v => calcFuelFromMass(v.toInt)).sum
      println(s"Day01 :: Part 1 = $part1")

      val part2 = data.map(v => calcCompensatedFuelFromMass(v.toInt)).sum
      println(s"Day01 :: Part 2 = $part2")
    }
  }

  /** Calculate fuel requirement based on mass.
   *
   *  @param mass module mass
   *  @return the fuel requirement for the mass
   */
  def calcFuelFromMass(mass: Int): Int = mass / 3 - 2

  /** Calculate fuel requirement for mass and total fuel mass.
   *
   *  @param mass module mass
   *  @return the fuel requirement for the mass and the total fuel mass
   */
  def calcCompensatedFuelFromMass(mass: Int): Int = {

    @tailrec
    def addFuel(fuel: List[Int]): List[Int] =
      fuel match {
        case x :: xs if x <= 0 => xs
        case x :: xs => addFuel(calcFuelFromMass(x) :: x :: xs)
      }

    val fuel = calcFuelFromMass(mass)
    addFuel(List(fuel)).sum
  }
}
