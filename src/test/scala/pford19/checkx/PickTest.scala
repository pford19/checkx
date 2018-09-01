package pford19.checkx

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers.check
import SomeNewGenerators._
import Stats._
import Combinatorics._


class PickTest extends FlatSpec {


  /** Nanoseconds and result of f. */
  def nanos[T](f: => T): (Long, T) = {
    val t0 = System.nanoTime()
    val result = f
    (System.nanoTime() - t0, result)
  }

  /** Nanoseconds only. */
  def nanosOnly(f: => Any): Long = nanos(f)._1


  def printSamples[T](g: Gen[T], n: Int) = {
    for (_ <- 1 to n) println(g.sample.get)
  }

  "pick" must "not repeat" in {

    val p1 = Gen.pick(2, 1 to 99)
    val p2 = Gen.pick(3, 1 to 3)
    check {
      forAll(p1) { ns => ns.size == 2 && ns.toSet.size == 2 }
    }
    check {
      forAll(p2) { ns => ns.size == 3 && ns.toSet.size == 3 }
    }

  }

  it must "may or may not preserve order" in {
    val g3_of99 = Gen.pick(3, 1 to 99)

    def isSorted(xs: Seq[Int]): Boolean = xs.sorted == xs

    def isNotSorted(xs: Seq[Int]): Boolean = !isSorted(xs)

    assert(
      SomeNewGenerators.streamFromGen(g3_of99).take(50).toList.exists(isSorted)
    )
    assert(
      SomeNewGenerators.streamFromGen(g3_of99).take(50).toList.exists(isNotSorted)
    )

  }


  /** (# permutations of m of n, # combinations of m of n). Results are doubles because values get large fast. */
  def permsAndCombs(m: Int, n: Int): (Double, Double) = {

    (Combinatorics.permutationCount(m, n), Combinatorics.combinationCount(m, n))
  }



  "pick" must "generate all combintations but not all permutations" in {


    def sampleAndAssert(m: Int, n: Int, g: Gen[Seq[Int]], samplesize: Int = 1000) = {
      require(0 <= m)
      require(m <= n)
      val (permutationCount, combinationCount) = permsAndCombs(m, n)

      val distinctSequences: Set[Seq[Int]] = streamFromGen(g).take(samplesize).toList.toSet
      val distinctSets: Set[Set[Int]] = distinctSequences.map(_.toSet)
      println(s"pick($m, 1 to $n) #samples=$samplesize #generated seqs=${distinctSequences.size}, #perms=$permutationCount, #generated sets=${distinctSets.size}, #subsets=$combinationCount")
      assert(m != 1 || distinctSequences.size == permutationCount, "m=1 implies all permutations")
      assert(m == 1 || distinctSequences.size < permutationCount, "m!=1 implies not all permutations")
      assert(distinctSets.size * 1.0 / combinationCount >= .80, "high percentage of combinations")
    }

    (1 to 10).foreach { m =>
      sampleAndAssert(m, 10, Gen.pick(m, 1 to 10))
    }

    val sampleSizes = List(500, 1000, 1500, 2000, 2500)
    sampleSizes.foreach { size =>
      sampleAndAssert(5, 10, Gen.pick(5, 1 to 10), size)
    }
  }

  /** First m of n values, if present, are at their input indexes in the result.
    * Any of the remaining n-m input values, if present at slot j, overwrite the
    * input value originally at slot j and the overwritten value is not present in the
    * result.
    */
  it must "have specific order semantics" in {

    def pickOrderProperty(m: Int, n: Int): Prop = {
      require(m >= 0)
      require(n >= m)
      val g = Gen.pick(m, 0 to n - 1)
      forAll(g) { picks => (0 to m - 1).forall(i => picks(i) == i || picks(i) >= m) }
    }

    for (n <- (0 to 10); m <- (0 to n)) {
      check(pickOrderProperty(m, n))
    }
  }


  def standardpick(n: Int, m: Int)(s: Seed): Iterable[Int] = SomeNewGenerators.standardPick(n, (1 to m).toVector)(s)

  def inorderpick(n: Int, m: Int)(s: Seed): Iterable[Int] = SomeNewGenerators.pickInOrder(n, (1 to m).toVector)(s)

  "standardpick" must "occasionally pick out of order" in {
    val s = streamof(standardpick(5, 10), Seed.random)
    assert(s.take(1000).exists(ints => ints.toList.sorted != ints.toList))
  }




  case class PickTimingData(pickSize: Int, inputSize: Int, runNumber: Int, runSize: Int, standardNanos: Long, inorderNanos: Long)

  /**
    *
    * @param pickSize  number of elements to pick from input
    * @param inputSize size of input iterable, specifically (1 to inputSize).toVector
    * @param runs      number of runs to perform
    * @param runsize   number of picks to perform per run
    * @param print     option, to print diagnostics per run
    * @return
    */
  private def comparePickTiming(pickSize: Int, inputSize: Int, runs: Int, runsize: Int, print: Boolean = true) = {
    val standardstream = streamof(standardpick(pickSize, inputSize))
    val inorderstream = streamof(inorderpick(pickSize, inputSize))


    val results = for (runNumber <- 0 to runs) yield {
      val standardTime = nanosOnly {
        standardstream.drop(runsize)
      }
      val inorderTime = nanosOnly {
        inorderstream.drop(runsize)
      }
      PickTimingData(pickSize, inputSize, runNumber, runsize, standardTime, inorderTime)
    }
    if (print) {
      results.filter(_.runNumber != 0).foreach { d =>
        println(s"${d.runSize} picks generated for ${d.pickSize} of ${d.inputSize}: standard=${d.standardNanos}, inorder=${d.inorderNanos}, ratio=${1.0 * d.inorderNanos / d.standardNanos}")
      }
    }
    results.toList.filter(_.runNumber != 0)
  }


  "standard pick" must "generate uniform results" in {

    def sample(n: Int, m: Int, sampleSize: Int) = {
      val s = streamof(standardpick(n, m))
      val dist = distribution(s.take(sampleSize).map(ints => ints.toList.sorted))

      val combs = combinationCount(n, m)

      val ratio = 1.0 * dist.size / combs
      val distStats = intStats(dist.values)
      println(f"standard pick, $n of $m, samplesize=$sampleSize, expected combinations=$combs%.4g, pcnt=${ratio * 100}%3.1f%% has these value stats: ${distStats.pretty}")
      val expect = if (combs < sampleSize) sampleSize / combs else 1.0
      assert(Math.abs(distStats.avg - expect) <= distStats.stddev) // expected within 1 stdev of observed
    }

    val n = 5
    for (m <- List(5, 10, 20, 30); c = combinationCount(n, m); if c < Int.MaxValue / 16) {
      sample(n, m, sampleSize = 4 * c.toInt)
    }
  }


  "orderedPick" must "respect size" in {
    for {
      n <- (1 to 10)
      m <- (2 * n to 10 * n by 5)
      g = SomeNewGenerators.orderedPick(n, (1 to m).toVector)
      p <- streamFromGen(g).take(100)
    } assert(p.size == n)

  }
  it must "preserve order" in {
    for {
      n <- (1 to 10)
      m <- (2 * n to 10 * n by 5)
      g = SomeNewGenerators.orderedPick(n, (1 to m).toVector)
      p <- streamFromGen(g).take(100)
    } assert(p.toList.sorted == p.toList)
  }

  it must "be uniformly distributed" in {
    //    for {
    //      n <- (1 to 10)
    //      m <- (2*n to 10*n by 5)
    //      g = DSLGenerators.orderedPick(n, (1 to m).toVector)
    //      (_,combos) = combinations(n, m)
    //      sample = Math.min(10000, 3*combos).toInt
    //      p = streamFromGen(g).take(sample)
    //    } {
    //      val expectedFrequency = Math.max(1.0, sample/combos)
    //      val stats = intStats(distribution(p).values)
    //      if(Math.abs(expectedFrequency - stats.avg) < stats.stddev) println(s"OOPS! expected=$expectedFrequency, got ${stats.pretty}")
    //      //assert(Math.abs(expectedFrequency - stats.avg) < stats.stddev)
    //    }
    for (
      n <- (1 to 20);
      m = 20;
      c = combinationCount(n, m) if c <= 100000.0
    ) {
      val sampleSize = 10 * c.toInt
      val g = SomeNewGenerators.orderedPick(n, (1 to m).toVector)
      val picks = streamFromGen(g).take(sampleSize)
      val histogram = distribution(picks)
      val frequencies = intStats(histogram.values)
      println(s"$n of $m: expected combinations=${c.toInt}, sample size=$sampleSize, stats on distinct combos: ${frequencies.pretty}")
      // .toList.sortBy(b=>(b.size, b)).foreach {
      //  b => println(s"$n of $m: ${b.toList}")
    }
  }

  "arbitrary long" must "be non-uniform" in {
    val sampleSize = 100000
    val x = distribution(streamFromGen(Arbitrary.arbitrary[Long]).take(sampleSize))
      .toList // of (Long, Int) = (arbitrary-long, frequency)
      .sortBy { case (ll, ff) => (-ff, ll) }
    x.take(10).foreach(p => println(s"high frequency Arbitrary[Long] $p"))
    val xxx = x.take(5).map(_._2).sum
    // the 5 most frequent values (-1, 0, 1, MinValue, MaxValue) comprise 49-51% of the results
    assert(.49 * sampleSize <= xxx)
    assert(xxx <=.51 * sampleSize)

  }


  "genFromIterator" must "work" in {
    val g = for (n <- Gen.choose(1, 10); m <- Gen.choose(1, n)) yield (n, m)
    check {
      forAll(g) { case (n, m) =>
        val s = streamFromGen(genFromIterator((1 to n).iterator))
        s.take(m).toList == (1 to m).toList
      }
    }
  }

  it must "fail when !hasNext" in {
    intercept[NoSuchElementException] {

      val tooLong = streamFromGen(genFromIterator(List(1, 2, 3, 4).iterator))
        .take(5) // still lazy
        .toList // reified ==> next on empty iterator ==> NoSuchElementException
    }

  }

  "xxx" must "work" in {
    val s: Stream[Long] = streamof((s: Seed) => s.long._1, Seed.random)
    val g1: Gen[Seed] = genFromSeed(Seed.random)
    val g2: Gen[Long] = genFromStream(s)
    val v1 = streamFromGen(g1).take(10).map(_.long).toSet
    val v2 = streamFromGen(g2).take(10).toSet
    val v3 = s.take(10).toSet
    List(v1, v2, v3).foreach(println)
    assert(v1.size == v2.size)
    assert(v2.size == v3.size)

  }

  "yyy" must "work" in {
    val s = streamof(identity[Seed])
    s.take(3).foreach(s => println(s"seed $s, ${s.long._1}, ${s.double._1}"))
    assert(s.take(3).map(_.long._1).toSet.size == 3)
    val gs = genFromIterator(s.iterator)

    val seed1 = gs.retryUntil(_ => true).sample.get
    val seed2 = gs.retryUntil(_ => true).sample.get
    println(s"seed1=$seed1, seed2=$seed2")
    assert(seed1 != seed2)
    val sgsn = streamFromGen(gs)
    assert(sgsn.take(3).map(_.long._1).toSet.size == 3)


  }


  "random long" must "be uniform" in {
    val sampleSize = 100
    val frequencies = distribution(streamFromGen(SomeNewGenerators.genRandomLong(0)).take(sampleSize))
      .toList // of (Long, Int) = (arbitrary-long, frequency)
      .sortBy { case (ll, ff) => (-ff, ll) }
    frequencies.take(10).foreach(p => println(s"high frequency almost arbitrary long $p"))
    val freqStats = stats(frequencies.map(_._2.toDouble))
    println(s"almost arbitrary long stats ${freqStats.pretty}")
    assert(1 == freqStats.med.toInt)
    assert(freqStats.stddev < .01)

  }

  it must "produce distinctive values" in {
    val g = genRandomLong(0)
    val s = streamFromGen(g)
    assert(s.take(100).toSet.size > 90) // expect 90% distinct
  }

  "stream chunks" must "work" in {
    val s = (1 to 10).toStream
    val t = streamGroups(5, s)
    assert(t.take(2).size == 2)
    assert(t.head == (1 to 5).toList)
    assert(t.tail.head == (6 to 10).toList)
  }

  it must "handle odd last chunk" in {
    val s = (1 to 9).toStream
    val t = streamGroups(5, s)
    assert(t.take(2).size == 2)
    assert(t.head == (1 to 5).toList)
    assert(t.tail.head == (6 to 9).toList)
  }


  "gen chunks" must "work" in {
    val g = Arbitrary.arbitrary[Int]
    val h = genGroups(4, g)
    val s = streamFromGen(h)
    val firsthree = s.take(3)
    firsthree.foreach(p => println(s"chunks of 2 $p"))
    assert(firsthree.size == 3)
    assert(firsthree.flatten.size == 12)
  }

  "genPermutations" must "permute uniformly" in {
    for (n <- (1 to 6)) {
      val g = SomeNewGenerators.genPermutation(('a' to 'z').toList.slice(0, n))
      val nperms = permutationCount(n, n).toInt
      val vals = streamFromGen(g).take(nperms * 5).toSet
      assert(vals.size >.95 * nperms) // hoping for at least 95%
    }
  }


}
