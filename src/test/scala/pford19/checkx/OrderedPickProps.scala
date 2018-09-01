package pford19.checkx

//import com.here.cme.validation.dsl.Combinatorics.combinations
//import com.here.cme.validation.dsl.SomeNewGenerators.streamof
//import com.here.cme.validation.dsl.Stats.intStats
//import com.navtech.`val`.validation.DeltaNavLinkViaAreaToMOAssocSetNavLinkValidationForGap
import org.scalacheck.Prop.{collect, exists, forAll}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Combinatorics._
import Stats._
import SomeNewGenerators._
import org.scalacheck.Test._


import scala.util.Try

/** Properties for the orderPick generator.
  * <ul>
  * <li>size is right
  * <li>preserves order
  * <li>picks without replacement
  * <li>bad index fails
  * <li>results are uniform
  * <li>elements are eventually picked.
  * </ul>
  *
  * For testing, a collection of 10 characters, `(a, b, c, d, e, f, g, h, i, j)` is used as the iput
  * and pick sizes are drawn from `(0, 1, 2, 5, 10)`.
  *
  *
  */
object OrderedPickProperties extends Properties("Ordered Pick Generator") {

  override def overrideParameters(p: Parameters): Parameters = p
    .withMinSuccessfulTests(25)
    .withMaxDiscardRatio(10.0f) // for


  private val pickSizes = List(0, 1, 2, 5, 10)
  val pickSizeGenerator = Gen.oneOf(pickSizes)
  val inputCollection = ('a' to ('a' + 9).toChar).toVector // first 10 characters

  def pickN(n: Int): Gen[Iterable[Char]] = SomeNewGenerators.orderedPick(n, inputCollection)

  // gen is a generator of pairs (n, picks) where
  // n is the expected size of picks
  // and picks is a random order pick from inputCollection

  val gen = for (n <- pickSizeGenerator; picks <- pickN(n)) yield (n, picks)


  // static fixture properties

  property("fixture property: input collection.size > 2") = inputCollection.size > 2

  property("fixture property: input collection is sorted") = inputCollection.sorted == inputCollection

  property("fixture property: input collection has unique values") = inputCollection.size == inputCollection.toSet.size

  // generator based properties of orderedPick

  property("ordered pick is expected size") = forAll(gen) { case (n, picks) =>
    collect(picks.headOption, picks.size) {
      n == picks.size
    }
  }

  property("ordered pick preserves input order") = forAll(gen) { case (_, picks) =>
    collect(picks.headOption, picks.size) {
      picks.toSeq.sorted == picks
    }
  }

  property("ordered pick picks without replacement") = forAll(gen) { case (_, picks) =>
    collect(picks.headOption, picks.size) {
      picks.toSet.size == picks.size
    }
  }

  /** Property that asserts that some pick of size `n` from `inputCollection` includes `c`.
    *
    * @param c arbitrary element of `inputCollections`
    * @param n pick size, 0 <= n <= inputCollection.size
    * @return Property
    */
  def picks(c: Char, n: Int): Prop = {
    require(inputCollection.contains(c))
    require(0 <= n && n <= inputCollection.size)
    exists(pickN(n)) { pick =>
      collect(c, pick) {
        pick.toSet.contains(c)
      }
    }
  }

  /** Interesting indices between 0 and m-1 inclusive. Interesting indices include first 2, last 2, and middle.
    * Middle is a pair if m is positive and even,  a single value if m is odd. Duplicates are eliminated.
    * Examples
    * {{{
    *   1 => 0
    *   2 => 0, 1
    *   3 => 0, 1, 2
    *   4 => 0, 1, 2, 3
    *   5 => 0, 1, 2, 3, 4
    *   6 => 0, 1, 2, 3, 4, 5
    *   7 => 0, 1, 3, 5, 6
    *   8 => 0, 1, 3, 4, 6, 7
    *   9 => 0, 1, 4, 7, 8
    * }}}
    *
    * @param m
    * @return
    */
  def interestingPicks(m: Int) = List(0, 1, m - 1, m - 2, Math.floor((m - 1) / 2.0).toInt, Math.ceil((m - 1) / 2.0).toInt).filter(j => 0 <= j && j < m).distinct.sorted

  for (m <- List(1, 2, 10);
       data = inputCollection.take(m);
       j <- interestingPicks(m);
       c = data(j);
       n <- List(1, 2, 5) if n <= m;
       g = SomeNewGenerators.orderedPick(n, data)
  ) {
    property(s"pick $n of $m contains $c") = exists(g) { picks => picks.toSet.contains(c) }
  }

  property("pick 0 of m has one distinct result") = {
    streamFromGen(SomeNewGenerators.orderedPick(0, inputCollection)).take(100).toSet.size == 1
  }

  property("pick m of m has one distinct result") = {
    streamFromGen(SomeNewGenerators.orderedPick(inputCollection.size, inputCollection)).take(100).toSet.size == 1
  }

  property("pick 1 of m has m distinct results") = {
    streamFromGen(SomeNewGenerators.orderedPick(1, inputCollection)).take(100).toSet.size == inputCollection.size
  }

  property("pick m-1 of m has m distinct results") = {
    streamFromGen(SomeNewGenerators.orderedPick(inputCollection.size - 1, inputCollection)).take(100).toSet.size == inputCollection.size
  }

  property("bad pick size fails to create generator") = {
    val tooSmall = Gen.choose(-10, -1)
    val tooBig = Gen.choose(inputCollection.size + 1, inputCollection.size + 10)
    Prop.forAllNoShrink(Gen.oneOf(tooSmall, tooBig)) { n => Try(SomeNewGenerators.orderedPick(n, inputCollection)).isFailure }

  }

  property("ordered pick generates uniform results") = {

    def sampleIsUniform(n: Int, m: Int, sampleSize: Int) = {
      val g = orderedPick(n, (1 to m).toVector)
      val s = streamFromGen(g)
      val dist = distribution(s.take(sampleSize))

      val combs = combinationCount(n, m)

      val ratio = 1.0 * dist.size / combs
      val distStats = intStats(dist.values)
      //println(f"pick in order, $n of $m, samplesize=$sampleSize, expected combinations=${combs.toInt}, pcnt=${ratio * 100}%3.1f%% has these value stats: ${distStats.pretty}")
      val expect = if (combs < sampleSize) sampleSize / combs else 1.0
      (Math.abs(distStats.avg - expect) <= distStats.stddev / 10) // expected within 1/10 stdev of observed average
    }

    val n = 5
    val sampleResults = for (m <- List(5, 10, 20, 30); c = combinationCount(n, m); if c < Int.MaxValue / 16) yield sampleIsUniform(n, m, sampleSize = 4 * c.toInt)
    sampleResults.forall(_ == true)
  }

  property("distributed") = forAll(orderedPick(1, (1 to 10))) { picks =>
    collect(picks) {
      true
    }
  }

  propertyWithSeed("orderedPick sometimes picks 3rd element", None) = exists(orderedPick(3, inputCollection)) { pick =>
    collect(pick) {
      pick.toSet.contains(inputCollection(2))
    }
  }

  val g1 = Arbitrary.arbitrary[Int]
  val g2 = g1.suchThat { n: Int => n % 255 == 0 }

  property("hard to prove") = forAll(g2) { n => n % 255 == 0 }

}
