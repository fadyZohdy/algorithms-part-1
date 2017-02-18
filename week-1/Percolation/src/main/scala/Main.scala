
object Main extends App {

  val n = readLine.toInt

  val p = Percolation(n)

  val r = scala.util.Random

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  time {
    1 to n*n foreach { _ =>
      val site = (r.nextInt(n)+1, r.nextInt(n)+1)
      p.open(site)
    }
  }
  println()
//   p.visualize
  println(s"number of open sites: ${p.numberOfOpenSites}")
  println(p.percolates)
}
