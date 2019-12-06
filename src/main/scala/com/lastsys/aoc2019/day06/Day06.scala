package com.lastsys.aoc2019.day06

import com.lastsys.aoc2019.util.{AocTask, loadResource}

import scala.util.chaining._

object Day06 extends AocTask {
  type Orbits = Map[String, Set[String]]
  type Graph = Map[String, Set[String]]

  override def run(): Unit = {
    val input = loadResource("day06.txt")

    input.foreach { data =>
      val orbits = parseOrbits(data)
      val part1 = countOrbits(orbits)
      println(s"Day06 :: Part1 = $part1")

      val part2 = shortestPath(orbits)("YOU", "SAN")
      println(s"Day06 :: Part2 = $part2")
    }
  }

  /** Parse orbit structure.
   *
   *  Store each parent orbit as a key in a map and its children in a set of children.
   *
   *  @param data input data
   *  @return orbit data structure
   */
  def parseOrbits(data: Seq[String]): Orbits = {
    data.foldLeft(Map.empty[String, Set[String]]) { (orbits, row) =>
      val (parent, child) = {
        val parts = row.split("\\)")
        (parts.head, parts.last)
      }

      (if (!orbits.contains(parent)) {
        // If we have a new key.
        orbits + (parent -> Set(child))
      } else {
        // If we have an existing key.
        orbits + (parent -> (orbits(parent) + child))
      }).pipe { o =>
        if (!orbits.contains(child)) {
          o + (child -> Set())
        } else o
      }
    }
  }

  /** Count all orbits by doing a depth first search.
   *
   *  @param orbits orbit tree data structure
   *  @return orbit data structure
   */
  def countOrbits(orbits: Orbits): Int = {

    def countTree(orbit: String, depth: Int = 1): Int = {
      val children = orbits(orbit)
      val childDepthSum = children.size * depth
      // Fold over the children of the given orbit.
      val subtreeDepthSum = children.foldLeft(0) { (sum, child) =>
        if (orbits.contains(child)) {
          // Branch.
          sum + countTree(child, depth + 1)
        } else {
          // Leaf.
          sum
        }
      }
      childDepthSum + subtreeDepthSum
    }

    countTree("COM")
  }

  /** Find the shortest path between start to end.
   *
   *  @param orbits orbit tree data structure
   *  @param start start orbit
   *  @param end end orbit
   *  @return shortest path length between the nodes
   */
  def shortestPath(orbits: Orbits)(start: String, end: String): Int = {

    val graph = convertOrbitsToGraph(orbits)

    def search(node: String, depth: Int = 1, visited: Set[String] = Set.empty): Int = {
      val updatedVisit = visited + node
      val neighbors = graph(node)
      if (neighbors.contains(end)) {
        depth
      } else {
        neighbors.foldLeft(Int.MaxValue) { (v: Int, neighbor: String) =>
          if (graph.contains(neighbor) && !updatedVisit.contains(neighbor)) {
            Math.min(search(neighbor, depth + 1, updatedVisit), v)
          } else v
        }
      }
    }

    search("YOU") - 2
  }

  /** Convert the orbits tree to a undirected graph.
   *
   *  @param orbits orbits tree structure
   *  @return undirected graph
   */
  def convertOrbitsToGraph(orbits: Orbits): Graph = {
    orbits.foldLeft(orbits) { case (o1, (key, values)) =>
      o1 ++ values.foldLeft(o1) { (o2, value) =>
          o2 + (value -> (o2(value) + key))
        }
      }
    }
}
