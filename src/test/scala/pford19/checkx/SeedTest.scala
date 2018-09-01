package pford19.checkx

import org.scalacheck.rng.Seed
import org.scalatest.FlatSpec

/**
  * Tests and experiments on [[org.scalacheck.rng.Seed]] random number generator class.
  */
class SeedTest extends FlatSpec {

  "Seed(0)" must "do the basics" in {
    val s = Seed(0)
    val nx = s.next
    val (n, s2) = s.long
    val (d, s3) = s.double

    assert(nx == s2)
    assert(nx == s3)

    println(s"seed=$s\nlong=$n\ndouble=$d\nnext=${nx}")
  }


  def longs(n: Int, seed: Seed): List[Long] = {
    def loop(n: Int, i: Int, seed: Seed, acc: List[Long]): List[Long] = {
      if (i > n)
        acc.reverse
      else {
        val (v, nxt) = seed.long
        loop(n, i + 1, nxt, v :: acc)
      }
    }

    loop(n, 0, seed, Nil)
  }

  "same seeds" must "generate same longs" in {
    val s = longs(20, Seed(0))
    val t = longs(20, Seed(0))
    assert(s == t)
  }

  "different seeds" must "generate different longs" in {
    val s = longs(20, Seed(0))
    val t = longs(20, Seed(1))
    assert(s != t)
  }

  /** Wrap a scalacheck.rng.Seed as an iterator over Long values. */
  def longIterator(s: Seed): Iterator[Long] = new Iterator[Long] {
    def hasNext: Boolean = true

    def next(): Long = {
      val (result, nextseed) = seed.long
      seed = nextseed
      result
    }

    private var seed = s
  }

  /** Wrap a scalacheck.rng.Seed as in iterator over Double values. */
  def doubleIterator(s: Seed): Iterator[Double] = new Iterator[Double] {
    def hasNext: Boolean = true

    def next(): Double = {
      val (result, nextseed) = seed.double
      seed = nextseed
      result
    }

    private var seed = s
  }

  /** Wrap a scalacheck.rng.Seed as an iterator over Seeds. */
  def seedIterator(s: Seed): Iterator[Seed] = new Iterator[Seed] {
    def hasNext: Boolean = true

    def next(): Seed = {
      seed = seed.next
      seed
    }

    private var seed = s
  }

  def longStream(s: Seed): Stream[Long] = {
    // could do this: Stream.cons(s.long._1, longStream(s.next)), but it inefficient because s.next is called twice
    val (head, rest) = s.long
    Stream.cons(head, longStream(rest))
  }

  def longStreamAlt(s: Seed): Stream[Long] = Stream.cons(s.long._1, longStreamAlt(s.next))

  "longIterator(Seed)" must "act like Iterator" in {
    val it = longIterator(Seed.random)
    println(longIterator(Seed.random).take(5).toSeq)
    assert(it.take(5).toSeq.size == 5)
    assert(it.take(5).toSeq != it.take(5).toSeq)
  }
  "longStream(Seed)" must "act like Stream" in {
    val st = longStream(Seed.random)
    println(st.map(_ % 10).take(5).toList)
  }

  "timing of longStream vs longStreamAlt" must "be interesting" in {
    val st1 = longStream(Seed(0))
    val st2 = longStreamAlt(Seed(0))

    def x[T](f: => T): (Long, T) = {
      val t0 = System.nanoTime()
      val result = f
      (System.nanoTime() - t0, result)
    }

    val timings = for (i <- 1 to 50;
      (time1, r1) = x {
        st1(100000)
      };
      (time2, r2) = x {
        st2(100000)
      }
    ) yield (i, (r1, r2), (time1, time2))

    for ( (i, (r1, r2), (time1, time2)) <- timings) {
      assert(r1 == r2)
      val altFaster = time2 <= time1
      println(s"$i: altFaster=$altFaster, t=$time1, alt-t=$time2, relative=${time1.toDouble/time2}")
      //assert(time1 < time2)
    }
    val xxx = timings.map(_._3).map(ts => ts._1 < ts._2).groupBy(identity).map{ case (k,vs) => (k, vs.size) }
    println(xxx)


  }
}