package kmeans

import scala.collection.parallel.ParSeq
import org.junit.Assert.assertEquals
import org.junit.Test

class MainParKMeansSuite extends KMeansSuite {
  def checkParKMeans(eta: Double,
                     points: ParSeq[Point],
                     means: ParSeq[Point],
                     expected: ParSeq[Point]): Unit = {
    val actual = kMeans(points, means, eta)
    val message = s"should work for 'eta': $eta and 'points': $points and 'means': $means" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message, expected.toList.sorted, actual.toList.sorted)
  }

  @Test def `'parKMeans should work for empty 'points' and empty 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq.empty[Point]
    val means: ParSeq[Point] = ParSeq.empty[Point]
    val expected: ParSeq[Point] = ParSeq.empty[Point]

    checkParKMeans(0.001, points, means, expected)
  }

  @Test def `'parKMeans' should work for empty 'points' and 1 'means'`: Unit = {
    val mean = new Point(1, 1, 1)
    val points: ParSeq[Point] = ParSeq.empty[Point]
    val means: ParSeq[Point] = ParSeq(mean)
    val expected: ParSeq[Point] = ParSeq(mean)

    checkParKMeans(0.001, points, means, expected)
  }

  @Test def `'parKMeans' should work for 4 'points' and 1 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val means: ParSeq[Point] = ParSeq(new Point(1, 1, 1))
    val expected: ParSeq[Point] = ParSeq(new Point(0, 0, 0))

    checkParKMeans(0.001, points, means, expected)
  }

  @Test def `'parKMeans' should work for 4 'points' and 2 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val means: ParSeq[Point] = ParSeq(new Point(0.5, 0, 0), new Point(-0.5, 0, 0))
    val expected: ParSeq[Point] = ParSeq(new Point(1, 0, 0), new Point(-1, 0, 0))

    checkParKMeans(0.001, points, means, expected)
  }

  @Test def `'parKMeans' should work for 4 'points' and Coursera 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq(
      new Point(0, 0,  1),
      new Point(0, 0, -1),
      new Point(0, 1,  0),
      new Point(0, 10, 0))

    val means: ParSeq[Point] = ParSeq(new Point(0, -1, 0), new Point(0, 2, 0))
    val expected: ParSeq[Point] = ParSeq(new Point(0, 0, 0), new Point(0, 5.5, 0))

    checkParKMeans(12.25, points, means, expected)
  }
}
