import edu.princeton.cs.algs4.StdDraw

/**
  * Created by droidman on 3/23/17.
  */
case class Point(val x: Double, val y: Double) extends Ordered[Point] {

  def draw: Unit = StdDraw.point(this.x, this.y)

  def drawTo(that: Point): Unit = StdDraw.line(this.x, this.y, that.x, that.y)

  /**
    * Returns the slope between this point and the specified point.
    * Formally, if the two points are (x0, y0) and (x1, y1), then the slope
    * is (y1 - y0) / (x1 - x0). For completeness, the slope is defined to be
    * +0.0 if the line segment connecting the two points is horizontal;
    * Double.POSITIVE_INFINITY if the line segment is vertical;
    * and Double.NEGATIVE_INFINITY if (x0, y0) and (x1, y1) are equal.
    *
    * @param  that the other point
    * @return the slope between this point and the specified point
    */
  def slopeTo(that: Point): Double = {
    if(this == that) Double.NegativeInfinity
    else if(this.x == that.x) Double.PositiveInfinity
    else if(this.y == that.y) 0.0
    else (that.y - this.y) / (that.x - this.x)
  }

  /**
    * Compares two points by y-coordinate, breaking ties by x-coordinate.
    * Formally, the invoking point (x0, y0) is less than the argument point
    * (x1, y1) if and only if either y0 < y1 or if y0 = y1 and x0 < x1.
    *
    * @param  that the other point
    * @return the value <tt>0</tt> if this point is equal to the argument
    *         point (x0 = x1 and y0 = y1);
    *         a negative integer if this point is less than the argument
    *         point; and a positive integer if this point is greater than the
    *         argument point
    */
  override def compare(that: Point): Int =
    if(this.x < that.x || (this.x == that.x && this.y < that.y)) -1 else 1

  /**
    * Compares two points by the slope they make with this point.
    * The slope is defined as in the slopeTo() method.
    *
    * @return the Comparator that defines this ordering on points
    */
  def slopeOrder(p1: Point, p2: Point): Ordered[Point] = ???



  override def toString = "(" + x + ", " + y + ")"
}
