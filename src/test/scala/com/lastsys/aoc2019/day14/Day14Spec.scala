package com.lastsys.aoc2019.day14

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.TableDrivenPropertyChecks

case class Example(reactions: Seq[String], result: Int)
object Example {
  def apply(reactions: String, result: Int): Example = {
    Example(reactions.stripMargin.split("\n").map(_.stripTrailing), result)
  }
}

class Day14Spec extends PropSpec with TableDrivenPropertyChecks with Matchers {

  val example1: Example = Example("""10 ORE => 10 A
                           |1 ORE => 1 B
                           |7 A, 1 B => 1 C
                           |7 A, 1 C => 1 D
                           |7 A, 1 D => 1 E
                           |7 A, 1 E => 1 FUEL""", 31)

  val example2: Example = Example("""9 ORE => 2 A
                           |8 ORE => 3 B
                           |7 ORE => 5 C
                           |3 A, 4 B => 1 AB
                           |5 B, 7 C => 1 BC
                           |4 C, 1 A => 1 CA
                           |2 AB, 3 BC, 4 CA => 1 FUEL""", 165)

  val example3: Example = Example("""157 ORE => 5 NZVS
                           |165 ORE => 6 DCFZ
                           |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
                           |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
                           |179 ORE => 7 PSHF
                           |177 ORE => 5 HKGWZ
                           |7 DCFZ, 7 PSHF => 2 XJWVT
                           |165 ORE => 2 GPVTF
                           |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""", 13312)

  val example4: Example = Example("""2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
                           |17 NVRVD, 3 JNWZP => 8 VPVL
                           |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
                           |22 VJHF, 37 MNCFX => 5 FWMGM
                           |139 ORE => 4 NVRVD
                           |144 ORE => 7 JNWZP
                           |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
                           |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
                           |145 ORE => 6 MNCFX
                           |1 NVRVD => 8 CXFTF
                           |1 VJHF, 6 MNCFX => 4 RFSQX
                           |176 ORE => 6 VJHF""", 180697)

  val example5: Example = Example("""171 ORE => 8 CNZTR
                           |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
                           |114 ORE => 4 BHXH
                           |14 VRPVC => 6 BMBT
                           |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
                           |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
                           |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
                           |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
                           |5 BMBT => 4 WPTQ
                           |189 ORE => 9 KTJDG
                           |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
                           |12 VRPVC, 27 CNZTR => 2 XDBXC
                           |15 KTJDG, 12 BHXH => 5 XCVML
                           |3 BHXH, 2 VRPVC => 7 MZWV
                           |121 ORE => 7 VRPVC
                           |7 XCVML => 6 RJRHP
                           |5 BHXH, 4 VRPVC => 5 LTCX""", 2210736)

  property("example 1 should be parsed properly") {
    Day14.parseReactions(example1.reactions) shouldBe Graph(
      Set("A", "B", "C", "D", "E", "ORE", "FUEL"),
      Map(
        ("ORE", "A") -> (10, 10),
        ("ORE", "B") -> (1, 1),
        ("A", "C") -> (7, 1),
        ("B", "C") -> (1, 1),
        ("A", "D") -> (7, 1),
        ("C", "D") -> (1, 1),
        ("A", "E") -> (7, 1),
        ("D", "E") -> (1, 1),
        ("A", "FUEL") -> (7, 1),
        ("E", "FUEL") -> (1, 1)
      ))
  }

  property("example 2 should be parsed properly") {
    Day14.parseReactions(example2.reactions) shouldBe Graph(
      Set("A", "B", "C", "AB", "BC", "CA", "ORE", "FUEL"),
      Map(
        ("ORE", "A") -> (9, 2),
        ("ORE", "B") -> (8, 3),
        ("ORE", "C") -> (7, 5),
        ("A", "AB") -> (3, 1),
        ("B", "AB") -> (4, 1),
        ("B", "BC") -> (5, 1),
        ("C", "BC") -> (7, 1),
        ("C", "CA") -> (4, 1),
        ("A", "CA") -> (1, 1),
        ("AB", "FUEL") -> (2, 1),
        ("BC", "FUEL") -> (3, 1),
        ("CA", "FUEL") -> (4, 1))
      )
  }
}
