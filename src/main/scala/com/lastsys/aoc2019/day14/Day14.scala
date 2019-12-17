package com.lastsys.aoc2019.day14

import com.lastsys.aoc2019.util.AocTask

import scala.collection.mutable

case class Graph(nodes: Set[String], edges: Map[(String, String), (Int, Int)])

object Day14 extends AocTask {
  override def run(): Unit = ???

  def parseReactions(reactions: Seq[String]): Graph = {
    val nodes = mutable.Set.empty[String]
    val edges = mutable.Map.empty[(String, String), (Int, Int)]

    reactions.foreach { row =>
      val a = row.split("=>").toSeq
      val (targetWeight, target) = {
        val b = a.last.trim.split(" ")
        (b(0).toInt, b(1))
      }
      val sources =
        a.head.split(",")
          .map(_.trim)
          .map(_.split(" "))
          .map(v => (v(0).toInt, v(1)))

      // First pass: extract nodes.
      nodes += target
      sources.foreach { case (_, name) => nodes += name }

      // Second pass: create edges with weights.
      sources.foreach { case (sourceWeight, name) =>
        val src = name
        val tgt = target
        edges += ((src, tgt) -> (sourceWeight, targetWeight))
      }
    }
    Graph(nodes.toSet, edges.toMap)
  }

  def calcRequirement(graph: Graph, product: String, source: String): Int = ???
}
