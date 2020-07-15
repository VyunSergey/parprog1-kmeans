package kmeans

import scala.collection.Seq
import org.junit.Assert.assertEquals
import org.junit.Test

class MainKMeansSuite extends KMeansSuite {
  def checkKMeans(eta: Double,
                  points: Seq[Point],
                  means: Seq[Point],
                  expected: Seq[Point]): Unit = {
    val actual = kMeans(points, means, eta)
    val message = s"should work for 'eta': $eta and 'points': $points and 'means': $means" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message, expected, actual)
  }

  @Test def `'kMeans should work for empty 'points' and empty 'means'`: Unit = {
    val points: Seq[Point] = Seq.empty[Point]
    val means: Seq[Point] = Seq.empty[Point]
    val expected: Seq[Point] = Seq.empty[Point]

    checkKMeans(0.001, points, means, expected)
  }

  @Test def `'kMeans' should work for empty 'points' and 1 'means'`: Unit = {
    val mean = new Point(1, 1, 1)
    val points: Seq[Point] = Seq.empty[Point]
    val means: Seq[Point] = Seq(mean)
    val expected: Seq[Point] = Seq(mean)

    checkKMeans(0.001, points, means, expected)
  }

  @Test def `'kMeans' should work for 4 'points' and 1 'means'`: Unit = {
    val points: Seq[Point] = Seq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val means: Seq[Point] = Seq(new Point(1, 1, 1))
    val expected: Seq[Point] = Seq(new Point(0, 0, 0))

    checkKMeans(0.001, points, means, expected)
  }

  @Test def `'kMeans' should work for 4 'points' and 2 'means'`: Unit = {
    val points: Seq[Point] = Seq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val means: Seq[Point] = Seq(new Point(0.5, 0, 0), new Point(-0.5, 0, 0))
    val expected: Seq[Point] = Seq(new Point(1, 0, 0), new Point(-1, 0, 0))

    checkKMeans(0.001, points, means, expected)
  }

  @Test def `'kMeans' should work for 4 'points' and Coursera 'means'`: Unit = {
    val points: Seq[Point] = Seq(
      new Point(0, 0,  1),
      new Point(0, 0, -1),
      new Point(0, 1,  0),
      new Point(0, 10, 0))

    val means: Seq[Point] = Seq(new Point(0, -1, 0), new Point(0, 2, 0))
    val expected: Seq[Point] = Seq(new Point(0, 0, 0), new Point(0, 5.5, 0))

    checkKMeans(12.25, points, means, expected)
  }
}
