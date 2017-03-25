import Fixtures.{Input9, Input8, Input6}
import org.scalatest.FlatSpec

/**
  * Created by droidman on 3/25/17.
  */
class FastSpec extends FlatSpec {

  "FastCollinearPoints" should "return correct set of line segments for 6 points" in {

    val fast = FastCollinearPoints(Set(
      Input6.p1,
      Input6.p2,
      Input6.p3,
      Input6.p4,
      Input6.p6
    ))

    assert(fast.segments == Set(
      LineSegment(Input6.p1, Input6.p4)
    ))

  }

  "FastCollinearPoints" should "return correct set of line segments for 8 points" in {

    val fast = FastCollinearPoints(Set(
      Input8.p1,
      Input8.p2,
      Input8.p3,
      Input8.p4,
      Input8.p5,
      Input8.p6,
      Input8.p7,
      Input8.p8
    ))

    assert(fast.segments == Set(
      LineSegment(Input8.p2, Input8.p1),
      LineSegment(Input8.p6, Input8.p5)
    ))
  }

  "FastCollinearPoints" should "return correct set of line segments for 9 points" in {

    val fast = FastCollinearPoints(Set(
      Input9.p1,
      Input9.p2,
      Input9.p3,
      Input9.p4,
      Input9.p5,
      Input9.p6,
      Input9.p7,
      Input9.p8,
      Input9.p9
    ))

    assert(fast.segments == Set(
      LineSegment(Input9.p9, Input9.p1)
    ))
  }

}

