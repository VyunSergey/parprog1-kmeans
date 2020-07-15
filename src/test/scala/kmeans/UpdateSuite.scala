package kmeans

import scala.collection.{Map, Seq}
import org.junit.Assert.assertEquals
import org.junit.Test

class UpdateSuite extends KMeansSuite {
  def checkUpdate(classified: Map[Point, Seq[Point]],
                  oldMeans: Seq[Point],
                  expected: Seq[Point]): Unit = {
    val actual = update(classified, oldMeans)
    val message = s"should work for 'classified': $classified and 'oldMeans': $oldMeans" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message, expected.sorted, actual.sorted)
  }

  @Test def `'update should work for empty 'classified' and empty 'oldMeans'`: Unit = {
    val classified: Map[Point, Seq[Point]] = Map.empty[Point, Seq[Point]]
    val oldMeans: Seq[Point] = Seq.empty[Point]
    val expected: Seq[Point] = Seq.empty[Point]

    checkUpdate(classified, oldMeans, expected)
  }

  @Test def `'update' should work for empty 'points' and 1 'means'`: Unit = {
    val mean = new Point(1, 1, 1)

    val classified: Map[Point, Seq[Point]] = Map.empty[Point, Seq[Point]]
    val oldMeans: Seq[Point] = Seq(mean)
    val expected: Seq[Point] = Seq(mean)

    checkUpdate(classified, oldMeans, expected)
  }

  @Test def `'update' should work for 4 'points' and 1 'means'`: Unit = {
    val points: Seq[Point] = Seq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val mean = new Point(1, 1, 1)

    val classified: Map[Point, Seq[Point]] = Map((mean, points))
    val oldMeans: Seq[Point] = Seq(mean)
    val expected: Seq[Point] = Seq(new Point(0, 0, 0))

    checkUpdate(classified, oldMeans, expected)
  }

  @Test def `'update' should work for 4 'points' and 2 'means'`: Unit = {
    val points: Seq[Point] = Seq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val mean1 = new Point(0, 0, 0)
    val mean2 = new Point(1, 1, 1)

    val classified: Map[Point, Seq[Point]] = Map((mean1, points.take(2)), (mean2, points.takeRight(2)))
    val oldMeans: Seq[Point] = Seq(mean1, mean2)
    val expected: Seq[Point] = Seq(new Point(1, 0, 0), new Point(-1, 0, 0))

    checkUpdate(classified, oldMeans, expected)
  }
}
