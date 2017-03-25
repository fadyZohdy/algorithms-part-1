/**
  * Created by droidman on 3/25/17.
  */
case class LineSegment(p: Point, q: Point) {

  def draw = p.drawTo(q)

  override def toString = p + " -> " + q

}
