import Fixtures.Input6
import org.scalatest.FlatSpec

/**
  * Created by droidman on 3/25/17.
  */
class BruteSpec extends FlatSpec {

  "BruteCollinearPoints" should "return correct set of line segments" in {

    val brute = BruteCollinearPoints(Set(
      Input6.p1,
      Input6.p2,
      Input6.p3,
      Input6.p4,
      Input6.p6
    ))

    assert(brute.segmentsImmutable == Set(
      LineSegment(Input6.p1, Input6.p4)
    ))
    assert(brute.segmentsMutable == Set(
      LineSegment(Input6.p1, Input6.p4)
    ))

  }

}
