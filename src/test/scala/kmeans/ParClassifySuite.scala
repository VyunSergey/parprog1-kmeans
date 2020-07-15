package kmeans

import scala.collection.parallel.{ParMap, ParSeq}
import org.junit.Assert.assertEquals
import org.junit.Test

class ParClassifySuite extends KMeansSuite {
  def checkParClassify(points: ParSeq[Point],
                       means: ParSeq[Point],
                       expected: ParMap[Point, ParSeq[Point]]): Unit = {
    val actual = classify(points, means)
    val message = s"should work for 'points': $points and 'means': $means" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message,
      expected.mapValues(_.toList.sorted).toList.sortBy(_._1),
      actual.mapValues(_.toList.sorted).toList.sortBy(_._1))
  }

  @Test def `'parClassify' should work for empty 'points' and empty 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq.empty[Point]
    val means: ParSeq[Point] = ParSeq.empty[Point]
    val expected: ParMap[Point, ParSeq[Point]] = ParMap.empty[Point, ParSeq[Point]]

    checkParClassify(points, means, expected)
  }

  @Test def `'parClassify' should work for empty 'points' and 1 'means'`: Unit = {
    val mean = new Point(1, 1, 1)
    val points: ParSeq[Point] = ParSeq.empty[Point]
    val means: ParSeq[Point] = ParSeq(mean)
    val expected: ParMap[Point, ParSeq[Point]] = ParMap((mean, ParSeq.empty[Point]))

    checkParClassify(points, means, expected)
  }

  @Test def `'parClassify' should work for 4 'points' and 1 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val mean = new Point(0, 0, 0)
    val means: ParSeq[Point] = ParSeq(mean)
    val expected: ParMap[Point, ParSeq[Point]] = ParMap((mean, points))

    checkParClassify(points, means, expected)
  }

  @Test def `'parClassify' should work for 4 'points' and 2 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, -1, 0),
      new Point(-1, 1, 0))

    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: ParSeq[Point] = ParSeq(mean1, mean2)
    val expected: ParMap[Point, ParSeq[Point]] = ParMap((mean1, points.take(2)), (mean2, points.reverse.take(2)))

    checkParClassify(points, means, expected)
  }
}
