package pford19.checkx

import org.scalacheck.rng.Seed

import scala.util.Random

case class SeedOps(wrapped: Seed) {

  /** An int value in the interval `[0, n)` determined by the `wrapped` Seed.
    * <p>
    * Ultimately depends on `java.util.Random.nextInt(n)`.
    *
    * @param n upper bound
    * @return random value in interval [0, n)
    */
  def int(n: Int): Int = {
    val scalaR: Random = new Random(wrapped.long._1)
    scalaR.nextInt(n)
  }


}
object SeedOps {
  implicit def fromSeed(wrapped: Seed): SeedOps = SeedOps(wrapped)
}
