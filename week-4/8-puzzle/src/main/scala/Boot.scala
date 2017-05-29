import java.io.File

import scala.io.Source

object Boot extends App{

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  val PATH = getClass.getResource("8puzzle").getPath

//  new File(PATH).listFiles.filter(_.isFile).toList.foreach{file =>
//    println(s"\n******************${file.getName}")
//    val lines = Source.fromFile(file).getLines().toArray
//
//    val dimension = lines.head.trim.toInt
//
//    val blocks = lines.tail.map{ line =>
//      line.trim.split(" ").filterNot(_ == "").map(_.trim.toInt)
//    }.filterNot(_.length<=1)
//
//    val initialBoard = new Board(blocks)
//
//    val solver = new Solver(initialBoard)
//
//    if(solver.isSolvable) {
//      time(solver.solution)
//      println(s"Minimum number of moves = ${solver.moves}")
//    }
//    else
//      println("No solution possible")
//  }

  println("please enter file name: ")
  val filename = readLine

  val lines = Source.fromFile(PATH + "/" + filename).getLines().toArray

  val dimension = lines.head.trim.toInt

  val blocks = lines.tail.map{ line =>
    line.trim.split(" ").filterNot(_ == "").map(_.trim.toInt)
  }.filterNot(_.length<=1)

  val initialBoard = new Board(blocks)

  val solver = new Solver(initialBoard)

  if(solver.isSolvable) {
    time(solver.solution)
    println(s"Minimum number of moves = ${solver.moves}")
  }
  else
    println("No solution possible")

}
