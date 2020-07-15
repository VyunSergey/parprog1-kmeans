package kmeans

import org.junit._

class KMeansSuite extends KMeans {
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(60 * 1000)
}
