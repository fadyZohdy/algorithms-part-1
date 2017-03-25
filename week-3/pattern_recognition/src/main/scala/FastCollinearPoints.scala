/**
  * Created by droidman on 3/25/17.
  */
case class FastCollinearPoints(points: Set[Point]) {

  def numberOfSegments: Int = getLinesWithPoints.size

  def segments: Set[LineSegment] = {
    getLinesWithPoints.map{s =>
     val sorted = mergeSort(s.toList)
     LineSegment(sorted(0), sorted(sorted.length-1))
    }
  }

  def getLinesWithPoints: Set[Set[Point]] = {
    points.foldLeft(Set.empty[Set[Point]]){(z, p) =>
      val slopes = getSlopes(p)
      val lines = getLines(slopes)
      lines.flatMap { line =>
        val completeLine = line ++ Set(p)
        if(completeLine.size < 4 || z.exists(s => completeLine.subsetOf(s))) z
        else Set(completeLine) ++ z
      }.toSet
    }
  }

  def getSlopes(point: Point) = {
    points.toList.map(p => (p, point.slopeTo(p)))
  }

  def getLines(pointsWithSlopes: List[(Point, Double)]): Iterable[Set[Point]] =
    pointsWithSlopes.groupBy(_._2).collect{ case (k, v) if v.length >= 3 => v.map(_._1).toSet }

  def mergeSort(points: List[Point]): List[Point] = {
    val n = points.length/2
    if(n == 0) points
    else {
      def merge(xs: List[Point], ys: List[Point], sorted: List[Point]): List[Point] = (xs, ys) match {
        case (xs, Nil) => sorted ++ xs
        case (Nil, ys) => sorted ++ ys
        case (x :: xt, y :: yt) =>
          if (x < y) merge(xt, ys, sorted :+ x)
          else merge(xs, yt, sorted :+ y)
      }

      val (l, r) = points.splitAt(n)
      merge(mergeSort(l), mergeSort(r), Nil)
    }
  }

}
