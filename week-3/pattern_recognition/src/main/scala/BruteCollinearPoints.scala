/**
  * Created by droidman on 3/25/17.
  */
case class BruteCollinearPoints(ps: Set[Point]) {

//  def numberOfSegments: Int = segments.size

  def segmentsImmutable: Set[LineSegment] = {
    val s = ps.subsets(4).foldLeft(Set.empty[Set[Point]])((z, l) => if (!z.exists(Set(l)) && isLine(l.toList)) z ++ Set(l) else z)
    s map { set =>
      set.toList.sorted match {
        case p1::p2::p3::p4::Nil => LineSegment(p1,p4)
      }
    }
  }

  def segmentsMutable: Set[LineSegment] = {
    var result = Set.empty[LineSegment]
    val sorted = ps.toArray.sorted
    for(i <- 0 to sorted.length-4) {
      for(j <- i+1 to sorted.length -3) {
        for(k <- j+1 to sorted.length -2) {
          for(m <- k+1 to sorted.length -1) {
            if (isLine(sorted(i)::sorted(j)::sorted(k)::sorted(m)::Nil))
              result = result ++ Set(LineSegment(sorted(i), sorted(m)))
          }
        }
      }
    }
    result
  }


  def isLine(ps: List[Point]): Boolean = ps match {
    case p1::p2::p3::p4::Nil =>
      if((p1.slopeTo(p2) == p1.slopeTo(p3)) && (p1.slopeTo(p2) == p1.slopeTo(p4))) true else false
    case m =>
      println(s"unexpected pattern $m"); false
  }

}
