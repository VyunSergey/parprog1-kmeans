package kmeans

import scala.collection.Seq
import org.junit.Assert.assertEquals
import org.junit.Test

class ConvergedSuite extends KMeansSuite {
  def checkConverged(eta: Double,
                     oldMeans: Seq[Point],
                     newMeans: Seq[Point],
                     expected: Boolean): Unit = {
    val actual = converged(eta, oldMeans, newMeans)
    val message = s"should work for 'eta': $eta and 'oldMeans': $oldMeans and 'newMeans': $newMeans" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message, expected, actual)
  }

  @Test def `'converged should work for empty 'oldMeans' and empty 'newMeans'`: Unit = {
    val oldMeans: Seq[Point] = Seq.empty[Point]
    val newMeans: Seq[Point] = Seq.empty[Point]

    checkConverged(0.001, oldMeans, newMeans, expected = true)
  }

  @Test def `'converged' should work for equal 'oldMeans' and 'newMeans'`: Unit = {
    val oldMeans: Seq[Point] = Seq(new Point(1, 1, 1))
    val newMeans: Seq[Point] = Seq(new Point(1, 1, 1))

    checkConverged(0.001, oldMeans, newMeans, expected = true)
  }

  @Test def `'update' should work for small 'oldMeans' and 'newMeans'`: Unit = {
    val oldMeans: Seq[Point] = Seq(new Point(1, 1, 1))
    val newMeans: Seq[Point] = Seq(new Point(1.000001, 1.000001, 1.000001))

    checkConverged(0.001, oldMeans, newMeans, expected = true)
  }

  @Test def `'update' should work for big 'oldMeans' and 'newMeans'`: Unit = {
    val oldMeans: Seq[Point] = Seq(new Point(1, 1, 1))
    val newMeans: Seq[Point] = Seq(new Point(1000, 1000, 1000))

    checkConverged(0.001, oldMeans, newMeans, expected = false)
  }
}
