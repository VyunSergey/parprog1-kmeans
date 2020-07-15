package kmeans

import scala.annotation.tailrec
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParIterable, ParMap, ParSeq}
import scala.util.Random
import org.scalameter._

class KMeans extends KMeansInterface {

  implicit val orderPoint: Ordering[Point] =
    (x: Point, y: Point) => ((x.x - y.x) + (x.y - y.y) + (x.z - y.z)).toInt

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to(mutable.ArrayBuffer)
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to(mutable.ArrayBuffer)
  }

  def findClosest(p: Point, means: IterableOnce[Point]): Point = {
    val it = means.iterator
    assert(it.nonEmpty)
    var closest = it.next()
    var minDistance = p.squareDistance(closest)
    while (it.hasNext) {
      val point = it.next()
      val distance = p.squareDistance(point)
      if (distance < minDistance) {
        minDistance = distance
        closest = point
      }
    }
    closest
  }

  /**
   * Classify point into means: each means gives the closets to it points
   * */
  def classify(points: Seq[Point], means: Seq[Point]): Map[Point, Seq[Point]] = {
    val grouped: Map[Point, Seq[Point]] =
      points
        .map(p => (p, findClosest(p, means)))
        .groupBy(_._2)
        .view.mapValues(_.map(_._1))
        .toMap

    means
      .map(mean => (mean, grouped.getOrElse(mean, Seq.empty[Point])))
      .toMap
  }

  /**
   * Classify point into means: each means gives the closets to it points
   * */
  def classify(points: ParSeq[Point], means: ParSeq[Point]): ParMap[Point, ParSeq[Point]] = {
    val grouped: ParMap[Point, ParSeq[Point]] =
      points
        .map(p => (p, findClosest(p, means)))
        .groupBy(_._2)
        .mapValues(_.map(_._1))
        .toMap

    means
      .map(mean => (mean, grouped.getOrElse(mean, ParSeq.empty[Point])))
      .toMap
  }

  /**
   * Finds the average of points if they are not empty or returns the old mean
   * */
  def findAverage(oldMean: Point, points: Seq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    val length = points.length
    points.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / length, y / length, z / length)
  }

  /**
   * Finds the average of points if they are not empty or returns the old mean
   * */
  def findAverage(oldMean: Point, points: ParSeq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    val length = points.length
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / length, y / length, z / length)
  }

  /**
   * Given old means and classified points computes the new means
   * */
  def update(classified: Map[Point, Seq[Point]], oldMeans: Seq[Point]): Seq[Point] = {
    oldMeans.map(mean => findAverage(mean, classified.getOrElse(mean, Seq.empty[Point])))
  }

  /**
   * Given old means and classified points computes the new means
   * */
  def update(classified: ParMap[Point, ParSeq[Point]], oldMeans: ParSeq[Point]): ParSeq[Point] = {
    oldMeans.map(mean => findAverage(mean, classified.getOrElse(mean, ParSeq.empty[Point])))
  }

  def distance(oldMeans: Iterable[Point], newMeans: Iterable[Point]): Double = {
    val pairs: Iterable[(Point, Point)] = oldMeans.zip(newMeans)
    val sumDist: Double = pairs.foldLeft(0.0)({case (acc, (p1, p2)) => acc + p1.squareDistance(p2)})
    if (pairs.isEmpty) sumDist else sumDist / pairs.size
  }

  def distance(oldMeans: ParIterable[Point], newMeans: ParIterable[Point]): Double = {
    val pairs: ParIterable[(Point, Point)] = oldMeans.zip(newMeans)
    val sumDist: Double = pairs.foldLeft(0.0)({case (acc, (p1, p2)) => acc + p1.squareDistance(p2)})
    if (pairs.isEmpty) sumDist else sumDist / pairs.size
  }

  /**
   * Check if new means are close enough to old means
   * */
  def converged(eta: Double, oldMeans: Seq[Point], newMeans: Seq[Point]): Boolean = {
    distance(oldMeans, newMeans) <= eta
  }

  /**
   * Check if new means are close enough to old means
   * */
  def converged(eta: Double, oldMeans: ParSeq[Point], newMeans: ParSeq[Point]): Boolean = {
    distance(oldMeans, newMeans) <= eta
  }

  /**
   * Main algorithm kMeans for sequential computation
   * */
  @tailrec
  final def kMeans(points: Seq[Point], means: Seq[Point], eta: Double): Seq[Point] = {
    val newMeans: Seq[Point] = update(classify(points, means), means)
    val dist: Boolean = converged(eta, means, newMeans)
/*
    println(s"In sequential kMeans" +
      s"\nold means: ${means.toList.mkString("[", ", ", "]")}" +
      s"\nnew means: ${newMeans.toList.mkString("[", ", ", "]")}" +
      s"\ndist:      ${distance(means, newMeans)}" +
      s"\nconverged: ${distance(means, newMeans)} <= $eta == ${distance(means, newMeans) <= eta}")
    println()
*/
    if (!dist) kMeans(points, newMeans, eta)
    else newMeans
  }

  /**
   * Main algorithm kMeans for parallel computation
   * */
  @tailrec
  final def kMeans(points: ParSeq[Point], means: ParSeq[Point], eta: Double): ParSeq[Point] = {
    val newMeans: ParSeq[Point] = update(classify(points, means), means)
    val dist: Boolean = converged(eta, means, newMeans)
/*
    println(s"In parallel kMeans" +
      s"\nold means: ${means.toList.mkString("[", ", ", "]")}" +
      s"\nnew means: ${newMeans.toList.mkString("[", ", ", "]")}" +
      s"\ndist:      ${distance(means, newMeans)}" +
      s"\nconverged: ${distance(means, newMeans)} <= $eta == ${distance(means, newMeans) <= eta}")
    println()
*/
    if (!dist) kMeans(points, newMeans, eta)
    else newMeans
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
  override def equals(obj: Any): Boolean = this.squareDistance(obj.asInstanceOf[Point]) == 0.0
}

object KMeansRunner {

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> false
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val kMeans = new KMeans()

    val numPoints = 1000000
    val eta = 0.001
    val k = 64
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }

    val parPoints = points.par
    val parMeans = means.par

    val partime = standardConfig measure {
      kMeans.kMeans(parPoints, parMeans, eta)
    }

    // Additional `println` to avoid bad interaction with JLine output
    println()
    println(s"sequential time: $seqtime")
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
    println()
  }

  // Workaround Dotty's handling of the existential type KeyValue
  implicit def keyValueCoerce[T](kv: (Key[T], T)): KeyValue = {
    kv.asInstanceOf[KeyValue]
  }
}
