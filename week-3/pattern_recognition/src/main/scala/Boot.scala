/**
  * Created by droidman on 3/23/17.
  */

import java.awt.Dimension

import edu.princeton.cs.algs4.StdDraw


import scala.io.Source
import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}

object Boot extends SimpleSwingApplication{

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  val PATH = getClass.getResource("collinear").getPath
  println("please enter file name: ")
  val filename = readLine

  val points = Source.fromFile(PATH + "/" + filename).getLines().foldLeft(Set.empty[Point]){(z,l) =>
    val coords = l.trim.split(" +")
    z ++ Set(Point(coords(0).trim.toDouble, coords(1).trim.toDouble))
  }

  val fast = FastCollinearPoints(points)

  val segments = time{fast.segments}

  def top = new MainFrame {
    contents = LinesPanel.ui(segments, points)
  }

  println(segments.size)

}
