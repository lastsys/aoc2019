package com.lastsys.aoc2019

import scala.io.Source
import scala.util.{Try, Using}

package object util {
  def loadResource(resourcePath: String): Try[Array[String]] =
    Using(Source.fromResource(resourcePath))(_.getLines.toArray)

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0)  / 1e6 ) + "ms")
    result
  }

  /** Increment value for a given key.
   *
   * If no value is present the value is assumed to be zero and
   * is replaced by one (initialization).
   *
   * @param map container
   * @param key key to increment value for
   * @tparam A key type
   * @return updated map
   */
  def incrementMap[A](map: Map[A, Int], key: A): Map[A, Int] =
    map + (key -> map.get(key).fold(1)(_ + 1))
}
