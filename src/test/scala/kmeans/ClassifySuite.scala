package kmeans

import scala.collection.{Map, Seq}
import org.junit.Assert.assertEquals
import org.junit.Test

class ClassifySuite extends KMeansSuite {
  def checkClassify(points: Seq[Point],
                    means: Seq[Point],
                    expected: Map[Point, Seq[Point]]): Unit = {
    val actual = classify(points, means)
    val message = s"should work for 'points': $points and 'means': $means" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message,
      expected.view.mapValues(_.toList.sorted).toList.sortBy(_._1),
      actual.view.mapValues(_.toList.sorted).toList.sortBy(_._1))
  }

  @Test def `'classify should work for empty 'points' and empty 'means'`: Unit = {
    val points: Seq[Point] = Seq.empty[Point]
    val means: Seq[Point] = Seq.empty[Point]
    val expected = Map.empty[Point, Seq[Point]]

    checkClassify(points, means, expected)
  }

  @Test def `'classify' should work for empty 'points' and 1 'means'`: Unit = {
    val mean = new Point(1, 1, 1)
    val points: Seq[Point] = Seq.empty[Point]
    val means: Seq[Point] = Seq(mean)
    val expected = Map[Point, Seq[Point]]((mean, Seq.empty[Point]))

    checkClassify(points, means, expected)
  }

  @Test def `'classify' should work for 4 'points' and 1 'means'`: Unit = {
    val points: Seq[Point] = Seq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val mean = new Point(0, 0, 0)
    val means: Seq[Point] = Seq(mean)
    val expected = Map((mean, points))

    checkClassify(points, means, expected)
  }

  @Test def `'classify' should work for 4 'points' and 2 'means'`: Unit = {
    val points: Seq[Point] = Seq(
      new Point(1, 1, 0),
      new Point(1, -1, 0),
      new Point(-1, 1, 0),
      new Point(-1, -1, 0))

    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: Seq[Point] = Seq(mean1, mean2)
    val expected = Map((mean1, points.take(2)), (mean2, points.takeRight(2)))

    checkClassify(points, means, expected)
  }
}
