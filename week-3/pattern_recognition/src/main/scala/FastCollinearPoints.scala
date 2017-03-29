
/**
  * Created by droidman on 3/25/17.
  */
case class FastCollinearPoints(points: Set[Point]) {

  def numberOfSegments: Int = getLinesWithPoints.size

  def segments: Iterable[LineSegment] = {
    getLinesWithPoints.map{s =>
     val sorted = s.toList.sorted
     LineSegment(sorted(0), sorted(sorted.length-1))
    }
  }

  def getLinesWithPoints: Iterable[Set[Point]] = {
    points.foldLeft(Iterable.empty[Set[Point]]){(z, p) =>
      val lines = getLines(p)
      lines.flatMap { line =>
        val completeLine = line ++ Set(p)
        if(completeLine.size < 4 && !isDuplicate(p, line)) z
        else Set(completeLine) ++ z
      }
    }
  }

  /**
    * check is this is the first point in the line or not
    * if this is not the first point then we have had this line before
    * because we iterate over the points in increasing order of their position
    *
    * @param p the anchor point
    * @param line the other points that form a line with p
    *  */
  def isDuplicate(p: Point, line: Set[Point]): Boolean = {
    line.forall(_ > p)
  }


  /**
    * group the points by their slope with the anchor point then return only the groups of size three or more
    * three is our minimum as the anchor point itself is not included in these groups
    * @param point the anchor point
    * @return sets of points with similar slope to the anchor point
    */
  def getLines(point: Point): Iterable[Set[Point]] =
    points.groupBy(o => point.slopeTo(o)).collect{ case (k, v) if v.size >= 3 => v }
}
