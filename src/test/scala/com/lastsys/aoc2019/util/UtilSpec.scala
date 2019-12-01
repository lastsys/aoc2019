package com.lastsys.aoc2019.util

import org.scalatest._
import com.lastsys.aoc2019.util

class UtilSpec extends WordSpec with Matchers {

  "loadResource" when {
    "text file is loaded" should {
      "have 5 lines" in {
        val r = util.loadResource("/data.txt")
        r.map(_.length should be (5))
      }
    }
  }
}
