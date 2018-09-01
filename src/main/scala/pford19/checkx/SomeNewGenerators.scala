package pford19.checkx

import org.scalacheck.Gen
import org.scalacheck.Gen.{const, frequency, listOfN}
import org.scalacheck.rng.Seed

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import SeedOps._

/**
  * Paul experiments and learnings with scalacheck generators.
  *
  * == Some Themes ==
  * <ul>
  * <li>Random number generators.
  * <li>Generators for [[org.scalacheck.rng.Seed]]
  * <li>Interoperations between Generators, Streams, and Iterators.
  * <li>order preserving pick
  * <li>permutations (shuffling)
  * <li>partitions
  * </ul>
  *
  * === Random Number Generators ===
  * === Seed Generators ===
  * === Generators, Streams, and Iterators ===
  * === Order Preserving Pick ===
  * === Permutations ===
  * === Partitions ===
  *
  * A <em>partition</em> of positive integer `n` is a sequence of positive integers that sum to `n`.
  *
  * So a simple representation is
  * {{{
  *   type Partition = Seq[Int]
  * }}}
  * where `n` is `head` of the sequence and the `tail` is the actual partition.
  * with the following semantics
  * {{{
  *   positiveProp = forAll { p: Partition => p.forall(_>0) }
  *   sizeProp = forAll { p: Partition => p.size >= 2 }
  *   sumProp = forAll { p: Partition => p.head == p.tail.sum }
  *   maxProp = forAll { p: Partition => p.tail.forall(_<p.head) }
  * }}}
  * Note that `maxProp` is redundant, it is implied by `sumProp`.
  * <p>
  * A partition of `n` can be used to divide a sequence of length `n` into a sequence of sub-sequences.
  * For example, the partition `(3, 1, 2)` partitions `List(a, b, c)` into `List(List(a), List(b,c))`.
  * <p>
  * This can be abstracted as {{{
  *   def partition[T] (p: Seq[Int], ts: Seq[T]): Seq[Seq[T]]
  *
  * }}}
  *
  */

object SomeNewGenerators {

  /**
    * Elements of `candidates` that sum up to `total`. Last value in `result` may be smaller than corresponding
    * element of `candidates` to ensure `result.sum == target`.
    * <p>
    * Example {{{
    *     trimToFit(6, List(1,2,3,4,5)) == List(1,2,3)
    *     trimToFit(7, List(1,2,3,4,5)) == List(1,2,3,1)
    * }}}
    *
    * @param total
    * @param candidates
    * @return initial sublist of candidates that sums total plus an additional elem if needed
    */

  def trimToFit(total: Int, candidates: List[Int]): List[Int] = {

    @tailrec
    def loop(candidates: List[Int], acc: List[Int], accsum: Int): List[Int] =
      candidates match {
        case Nil => acc.reverse
        case h :: r if accsum + h >= total => (total - accsum :: acc).reverse
        case h :: r if accsum + h < total => loop(r, h :: acc, h + accsum)
      }

    loop(candidates, Nil, 0)
  }

  /** Partitions of integer `n`.
    *
    * A partition of `n` is a sequence of positive integers that sums to `n`. If `n <= 0' the
    * result is the empty list.
    *
    * `maxPartitionSize` limits the size of the result. For example
    * {{{
    *   genPartitions(10, maxpartitionSize = 3)
    * }}}
    * could generate results such as
    * {{{
    *   1, 3, 6
    *   3, 3, 4
    *   8, 2
    *   10
    * }}}
    * but never
    * {{{
    *   1, 2, 3, 4
    * }}
    *
    * @param n                Number to partition, possibly 0
    * @param maxPartitionSize max partition size, ignored if > n
    * */
  def genPartitionsOf(n: Int, maxPartitionSize: Int): Gen[List[Int]] = {

    if (n <= 0) {
      const(Nil)
    } else {
      val sizes = (1 to Math.min(n, maxPartitionSize)).toList
      val freqs = sizes.reverse.map(n => Math.sqrt(100.0 * n).toInt)
      val freqPairs = freqs.zip(sizes).map { case (f, c) => (f, const(c)) }
      val h = frequency(freqPairs: _*)
      for {
        ns <- listOfN(n, h)
      } yield trimToFit(n, ns)
    }
  }

  /** Permutations of `n`.
    *
    * A permutation is a shuffling of the sequence (0 to n-1).
    *
    * @param n size of the permutation.
    * @return a permutation of `n`, a shuffling of `(0 to n-1)`
    **/

  def genPermutation(n: Int) = Gen.const(n).map(n => scala.util.Random.shuffle((0 to n - 1).toVector))


  /**
    * Seed-based (deterministic) permutation of `n`, a shuffling of sequence `(0 to n-1)`.
    * <p>
    * The permutation is `n-1` random swaps. The seed is advanced once per swap.
    *
    * @return permutation and advanced seed
    */
  def seededPermutation(n: Int, seed0: Seed): (Seq[Int], Seed) = {

    val buff = new ArrayBuffer[Int]()
    (0 to n - 1).foreach(buff += _)

    def swap(j: Int, k: Int) = {
      if (j != k) {
        val t = buff(j)
        buff(j) = buff(k)
        buff(k) = t
      }
    }

    // Recursive loop to permute buff with swaps, returning the advanced seed.
    @tailrec def loop(m: Int, seed: Seed): Seed = {
      require(m >= 0)
      if (m <= 1)
        seed
      else {
        val (j, s) = randomInt(m, seed)
        swap(j, m - 1) // mutate buff
        loop(m - 1, s)
      }
    }

    val advancedSeed = loop(n, seed0)
    (buff, advancedSeed)
  }

  /** Integer in the interval `[0, bound)` based on `seed.long` paired with next seed.
    */
  def randomInt(bound: Int, seed: Seed): (Int, Seed) = {
    require(bound > 0)
    (seed.int(bound), seed.next)
  }


  /** Random permutations of `coll`.
    *
    * @see genPermutations
    */
  def genPermutation[T](coll: Iterable[T]): Gen[Iterable[T]] = for (p <- genPermutation(coll.size)) yield permute(p, coll)

  /** `coll` permuted by `p`.
    *
    * `p` is a shuffling of (0 to n-1) where `n == p.size == coll.size`.
    *
    * The result has the defining property {{{
    *   for (j <- 0 to n-1) assert(coll(i) == result(p(i)))
    * }}}
    *
    * Some examples
    * {{{
    *   permute(0 to 3,           List(a, b, c, d)) == List(a, b, c, d) // the identity permutation
    *   permute(3 to 0 by -1,     List(a, b, c, d)) == List(d, c, b, a) // the reverse permutation
    *   permute(List(0, 3, 2, 1), List(a, b, c, d)) == List(a, d, c, b) // swap indexes 1 and 3
    * }}}
    *
    * @param p    a permutation, a shuffling of (0 to n-1)
    * @param coll any collection of size n
    * @tparam T type of coll
    * @return A sequence that is coll permutated by `p`
    */
  def permute[T](p: Seq[Int], coll: Iterable[T]): Seq[T] = {
    val buff = new Array[Option[T]](p.size)

    @tailrec
    def loop(p: Seq[Int], coll: Iterable[T]): Unit = {
      if (p.size == 0)
        ()
      else {
        assert(buff(p.head) == null)
        buff(p.head) = Some(coll.head)
        loop(p.tail, coll.tail)
      }
    }

    require(p.size == coll.size)
    loop(p, coll)
    val result = buff.map(_.get).toSeq // not quite sure why, but .flatten doesn't compile
    assert(result.size == p.size)
    result
  }

  /** A proto-generator that picks a given number of elements from a list, randomly,
    * based on `seed`. Copies logic from scalacheck.Gen.pick.
    *
    * @param n     number of elements to pick
    * @param coll  collection to pick from
    * @param seed0 initial seed
    * @tparam T element type
    * @return random subset of coll` of size `n`, determined by `seed`=
    **/
  def standardPick[T](n: Int, coll: Iterable[T])(seed0: Seed): Iterable[T] = {
    def standardPick2[T](n: Int, l: Iterable[T])(seed0: Seed): (Iterable[T], Seed) = {
      if (n > l.size || n < 0) throw new IllegalArgumentException(s"invalid choice: $n")
      else if (n == 0) (Nil, seed0)
      else {
        val buf = ArrayBuffer.empty[T]
        val it = l.iterator
        var seed = seed0
        var count = 0
        while (it.hasNext) {
          val t = it.next
          count += 1
          if (count <= n) {
            buf += t
            //assert(buf.size <= n)
          } else {
            val (x, s) = seed.long
            val i = (x & 0x7fffffff).toInt % count
            if (i < n) {
              buf(i) = t
              //assert(buf.size == n)
            }
            seed = s
          }
        }
        (buf, seed)
      }
    }

    standardPick2(n, coll)(seed0)._1
  }

  /** A proto-generator that picks a given number of elements from a list, randomly,
    * based on `seed`. The elements in the result are in the same order as in `coll`
    *
    * @param n     number of elements to pick
    * @param coll  collection to pick from
    * @param seed0 initial seed
    * @tparam T element type
    * @return random subset of coll` of size `n`, determined by `seed`, with order preserved
    **/

  def pickInOrder[T](n: Int, coll: Iterable[T])(seed0: Seed): Iterable[T] = {
    def pickInOrder2[T](n: Int, l: Iterable[T])(seed0: Seed): (Iterable[T], Seed) = {
      if (n > l.size || n < 0) throw new IllegalArgumentException(s"invalid choice: $n")
      else if (n == 0) (Nil, seed0)
      else {
        val buf = ArrayBuffer.empty[T]
        val it = l.iterator
        var seed = seed0
        var count = 0
        while (it.hasNext) {
          val t = it.next
          count += 1
          if (count <= n) {
            buf += t
            //assert(buf.size <= n)
          } else {
            val (x, s) = seed.long
            val i = (x & 0x7fffffff).toInt % count
            if (i < n) {
              if (i == n - 1)
                buf(i) = t // save one buff operation when slot to overwrite is also the last
              else {
                buf.remove(i) // remove old value in slot i, shrinking buff to size n-1
                buf += t // append replacement value t, preserving input order, returning buff to size n
              }
              //assert(buf.size == n)
            }
            seed = s
          }
        }
        (buf, seed)
      }
    }

    pickInOrder2(n, coll)(seed0)._1
  }

  /** Random pick `n` elements of `coll`, preserving order in `coll`.
    * <p>
    * A variant of `pick` from [[org.scalacheck.Gen]].
    *
    * @param n    number of elements to pick, n < coll.size
    * @param coll collection from which to pick
    * @tparam T type of collection element
    * @return Generator for `n` elements of `coll` selected at random, preserving order
    */
  def orderedPick[T](n: Int, coll: Iterable[T]): Gen[Iterable[T]] = {
    require(0 <= n)
    require(n <= coll.size)
    genFromSeed(Seed.random).map(pickInOrder(n, coll))
  }

  /** A generator from a stream.
    * <p>
    * The sequence of elements from the generator is the same as the stream.
    * <p>
    * If the stream has a defined (finite) size, then the generator will eventually
    * fail with a `NoSuchElementExpection`.
    *
    * @param stream input stream
    * @tparam T type of stream element
    * @return a generator wrapping  `stream`
    */
  def genFromStream[T](stream: Stream[T]): Gen[T] = genFromIterator(stream.iterator)


  /** A generator from an iterator.
    * <p>
    * The sequence of elements from the generator is the same as the iterator.
    * <p>
    * If the iterator has a defined (finite) size, then the generator will eventually
    * fail with a `NoSuchElementException`.
    *
    * @param iterator input iterator
    * @tparam T type of iterator element
    * @return a generator wrapping `iterator`
    */
  def genFromIterator[T](iterator: Iterator[T]): Gen[T] = Gen.const(0).map(_ => iterator.next)


  def genFromSeed(seed0: Seed = Seed.random): Gen[Seed] = {
    val x = streamof(identity[Seed], seed0)
    genFromStream(x)
  }

  /** Stream yields successive seeds with initial seed `seed0`.
    * <p>
    * Sequence of stream values is equivalent to `(seed0, seed0.next, seed0.next.next, ...)`.
    *
    * @param seed0 initial seed, defaults to Seed.random
    * @return Stream that yields successive seeds
    */
  def seedStream(seed0: Seed = Seed.random): Stream[Seed] = Stream.cons(seed0, seedStream(seed0.next))

  /** A stream based on a seed and function of seed.
    * <p>
    * The resulting stream has this structure {{{
    *   (f(seed0), f(seed0.next), f(seed0.next.next), ...)
    *   == seedStream(seed0).map(f)
    * }}}
    *
    * @param f     value generating function
    * @param seed0 initial seed
    * @tparam T return type of `f`
    * @return stream based on `f` and `seed0``
    **/
  def streamof[T](f: Seed => T, seed0: Seed = Seed.random): Stream[T] = seedStream(seed0).map(f)


  /** Stream from a generator.
    * <p>
    * The stream will retry the generator to ensure successive values.
    *
    * Relies [[Gen.retryUntil()]], which will throw an exception if
    * it cannot generate a value within a certain number of tries (currently
    * 10000, (1.14)).
    *
    * <p>
    * Note that the result is a `Stream`, not a generator. In contrast `Gen.infiniteStream`
    * returns a `Gen[Stream[T]` from a `Gen[T]`. That is, the
    * two methods have these distinct signatures
    * {{{
    *   streamFromGen[T]:      Gen[T] => Stream[T]
    *   Gen.infiniteStream[T]: Gen[T] => Gen[Stream[T]]
    * }}}
    *
    * @param gen input generator
    * @tparam T element type
    * @return stream with same element sequence of generator
    */
  def streamFromGen[T](gen: Gen[T]): Stream[T] = Stream.cons(gen.retryUntil(_ => true).sample.get, streamFromGen(gen))

  /** Random and uniformly distributed long values in the inclusive range Long.MinValue, Long.MaxValue.
    * <p>
    * Based on [[org.scalacheck.rng.Seed]]
    * <p>
    * Unlike [[org.scalacheck.Arbitrary]]`.arbitrary[Long]` this generator has no bias toward the distinctive values
    * {{{
    * 0L, 1L, -1L, Long.MinValue, Long.MaxValue.
    * }}}
    * They are equally likely as any other `Long` value: that is, extremely unlikely.
    *
    * @param longSeed Initial value, to generates `Seed(longSeed)`
    * @return generator of uniformly random Long values
    */
  def genRandomLong(longSeed: Long): Gen[Long] = genFromSeed(Seed(longSeed)).map(_.long._1)

  /** Random and uniformly distributed doubles in the range [0.0, 1.0). Based on [[org.scalacheck.rng.Seed]].
    * <p>
    * Based on [[org.scalacheck.rng.Seed]]
    * <p>
    * Unlike [[org.scalacheck.Arbitrary]]`.arbitrary[Double]` this generator has no bias toward the distinctive values
    * {{{
    * 0L, 1L, -1L, S.MinValue, Long.MaxValue.
    * }}}
    * They are equally likely as any other `Long` value: that is, extremely unlikely.
    *
    * @param longSeed Initial value, to generates `Seed(longSeed)`
    * @return generator of uniformly random Long values
    */
  def defRandomDouble(longSeed: Long) = genFromSeed(Seed(longSeed)).map(_.double._1)

  /** Uniformly random `Long` values.
    * <p>
    * Based on [[org.scalacheck.rng.Seed]].
    * <p
    *
    * @param seed initial Seed
    * @return generator of random `Long` values
    */
  def genRandomLong(seed: Seed) = genFromSeed(seed).map(_.long._1)

  /** Uniformly random `Double` values, in the range `[0.0, 1.0)`.
    * <p>
    * Based on [[org.scalacheck.rng.Seed]].
    * <p
    *
    * @param seed initial Seed
    * @return generator of random `Double` values
    */
  def genRandomDouble(seed: Seed) = genFromSeed(seed).map(_.double._1)

  /** A stream of `n`-length groups from a stream.
    * <p>
    * Example
    * {{{
    *     val s: Stream = (1 to 6).toStream
    *     val c = streamGroups(3, s)
    *     assert(c.head == List(1, 2, 3))
    *     assert(c.tail.head == List(4, 5, 6))
    * }}}
    *
    * @param n chunk size
    * @param s input stream
    * @tparam T element type
    * @return Stream of n-sized runs.
    */
  def streamGroups[T](n: Int, s: Stream[T]): Stream[List[T]] = {
    def groups(s: Stream[T]): Stream[List[T]] = Stream.cons(s.take(n).toList, groups(s.drop(n)))

    groups(s)
  }

  /** A generator of `n`-length groups from a generator.
    * <p>
    * Example
    * {{{
    *     val g: Generator = (1 to 6).toGenerator
    *     val c = generator(3, s)
    *     val s = streamFromGen(c)
    *     assert(s.head == List(1, 2, 3))
    *     assert(s.tail.head == List(4, 5, 6))
    * }}}
    *
    * @param n chunk size
    * @param g input generator
    * @tparam T element type
    * @return generator of n-sized chunks form `g`
    */
  def genGroups[T](n: Int, g: Gen[T]): Gen[List[T]] = genFromStream(streamGroups(n, streamFromGen(g)))

}
