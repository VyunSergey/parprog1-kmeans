package kmeans

import scala.collection.parallel.{ParMap, ParSeq}
import org.junit.Assert.assertEquals
import org.junit.Test

class ParUpdateSuite extends KMeansSuite {
  def checkParUpdate(classified: ParMap[Point, ParSeq[Point]],
                     oldMeans: ParSeq[Point],
                     expected: ParSeq[Point]): Unit = {
    val actual = update(classified, oldMeans)
    val message = s"should work for 'classified': $classified and 'oldMeans': $oldMeans" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message, expected.toList.sorted, actual.toList.sorted)
  }

  @Test def `'parUpdate should work for empty 'classified' and empty 'oldMeans'`: Unit = {
    val classified: ParMap[Point, ParSeq[Point]] = ParMap.empty[Point, ParSeq[Point]]
    val oldMeans: ParSeq[Point] = ParSeq.empty[Point]
    val expected: ParSeq[Point] = ParSeq.empty[Point]

    checkParUpdate(classified, oldMeans, expected)
  }

  @Test def `'parUpdate' should work for empty 'points' and 1 'means'`: Unit = {
    val mean = new Point(1, 1, 1)

    val classified: ParMap[Point, ParSeq[Point]] = ParMap.empty[Point, ParSeq[Point]]
    val oldMeans: ParSeq[Point] = ParSeq(mean)
    val expected: ParSeq[Point] = ParSeq(mean)

    checkParUpdate(classified, oldMeans, expected)
  }

  @Test def `'parUpdate' should work for 4 'points' and 1 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val mean = new Point(1, 1, 1)

    val classified: ParMap[Point, ParSeq[Point]] = ParMap((mean, points))
    val oldMeans: ParSeq[Point] = ParSeq(mean)
    val expected: ParSeq[Point] = ParSeq(new Point(0, 0, 0))

    checkParUpdate(classified, oldMeans, expected)
  }

  @Test def `'parUpdate' should work for 4 'points' and 2 'means'`: Unit = {
    val points: ParSeq[Point] = ParSeq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val mean1 = new Point(0, 0, 0)
    val mean2 = new Point(1, 1, 1)

    val classified: ParMap[Point, ParSeq[Point]] = ParMap((mean1, points.take(2)), (mean2, points.reverse.take(2)))
    val oldMeans: ParSeq[Point] = ParSeq(mean1, mean2)
    val expected: ParSeq[Point] = ParSeq(new Point(1, 0, 0), new Point(-1, 0, 0))

    checkParUpdate(classified, oldMeans, expected)
  }
}
