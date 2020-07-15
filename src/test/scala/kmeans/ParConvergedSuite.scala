package kmeans

import scala.collection.parallel.ParSeq
import org.junit.Assert.assertEquals
import org.junit.Test

class ParConvergedSuite extends KMeansSuite {
  def checkParConverged(eta: Double,
                        oldMeans: ParSeq[Point],
                        newMeans: ParSeq[Point],
                        expected: Boolean): Unit = {
    val actual = converged(eta, oldMeans, newMeans)
    val message = s"should work for 'eta': $eta and 'oldMeans': $oldMeans and 'newMeans': $newMeans" +
      s"\n'expected': $expected\n'actual'  : $actual\n"
    assertEquals(message, expected, actual)
  }

  @Test def `'converged should work for empty 'oldMeans' and empty 'newMeans'`: Unit = {
    val oldMeans: ParSeq[Point] = ParSeq.empty[Point]
    val newMeans: ParSeq[Point] = ParSeq.empty[Point]

    checkParConverged(0.001, oldMeans, newMeans, expected = true)
  }

  @Test def `'converged' should work for equal 'oldMeans' and 'newMeans'`: Unit = {
    val oldMeans: ParSeq[Point] = ParSeq(new Point(1, 1, 1))
    val newMeans: ParSeq[Point] = ParSeq(new Point(1, 1, 1))

    checkParConverged(0.001, oldMeans, newMeans, expected = true)
  }

  @Test def `'update' should work for small 'oldMeans' and 'newMeans'`: Unit = {
    val oldMeans: ParSeq[Point] = ParSeq(new Point(1, 1, 1))
    val newMeans: ParSeq[Point] = ParSeq(new Point(1.000001, 1.000001, 1.000001))

    checkParConverged(0.001, oldMeans, newMeans, expected = true)
  }

  @Test def `'update' should work for big 'oldMeans' and 'newMeans'`: Unit = {
    val oldMeans: ParSeq[Point] = ParSeq(new Point(1, 1, 1))
    val newMeans: ParSeq[Point] = ParSeq(new Point(1000, 1000, 1000))

    checkParConverged(0.001, oldMeans, newMeans, expected = false)
  }
}
