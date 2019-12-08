package com.lastsys.aoc2019.day08

import com.lastsys.aoc2019.util.{AocTask, loadResource}

import scala.annotation.tailrec



object Day08 extends AocTask {
  type Image = Seq[Seq[Int]]

  override def run(): Unit = {
    val input = loadResource("day08.txt")

    input.foreach { data =>
      val image = parseImage(data.head, 25, 6)
      part1(image)

      println("Day08 :: Part2 = ")
      render(flatten(image), 25, 6).foreach(println)
    }
  }

  def parseImage(data: String, width: Int, height: Int): Image = {
    @tailrec
    def extractLayers(data: String, image: Image): Image = {
      if (data.isEmpty) {
        image
      } else {
        val chunk = data.substring(0, width * height)
        val chunkDigits = chunk.map(_.asDigit)
        extractLayers(data.substring(width * height), image :+ chunkDigits)
      }
    }
    extractLayers(data, Seq.empty[Seq[Int]])
  }

  def part1(image: Image): Unit = {
    var layer = 0
    var zeroCount = Int.MaxValue
    image.zipWithIndex.foreach { case (l, i) =>
      val zeros = l.count(_ == 0)
      if (zeros < zeroCount) {
        zeroCount = zeros
        layer = i
      }
    }
    val ones = image(layer).count(_ == 1)
    val twos = image(layer).count(_ == 2)
    println(s"Day 08 :: Part1 = ${ones * twos}")
  }

  def flatten(image: Image): Seq[Int] = {
    image.foldLeft(Seq.empty[Int]) { case (img, layer) =>
      if (img.isEmpty) layer else {
        img.zip(layer).map { case (p1, p2) =>
          if (p1 == 2) p2 else p1
        }
      }
    }
  }

  def render(flattenedImage: Seq[Int], width: Int, height: Int): Seq[String] = {
    @tailrec
    def renderRow(img: Seq[Int], row: Int = 0, lines: Seq[String] = Seq.empty): Seq[String] = {
      if (row == height) lines else {
        val r = img.take(width).map {
          case 0 => " "
          case 1 => "X"
          case 2 => " "
        }
        renderRow(img.drop(width), row + 1, lines :+ r.mkString)
      }
    }
    renderRow(flattenedImage)
  }
}
