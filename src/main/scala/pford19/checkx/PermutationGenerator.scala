package pford19.checkx

import org.scalacheck.Gen

object PermutationGenerator {

  /**
    * Permutations of `n`, that is, permutations of `Seq(0 to n-1)`.
    * <p>
    * The distribution is not uniform.
    * <ul>
    * <li>1/14 are identity or reverse
    * <li>1/7=2/14 are a rotation, including the identity
    * <li>1/7=2/14 are a reverse rotation, including reverse
    * <li>1/7=2/14 are a swap
    * <li>1/2=7/17 are a random permutation, including any of the above
    *
    * @param n
    * @return
    */
  def weightedPermutations(n: Int): Gen[Permutation] = {
    val pg = PermGroup(n)
    Gen.frequency(
      (1, Gen.oneOf(List(pg.identity, (pg.reverse)))),
      (2, Gen.oneOf(pg.rotations)),
      (2, Gen.oneOf(pg.reverseRotations)),
      (2, Gen.oneOf(pg.transpositions)),
      (2, maximalSwaps(n)),
      (7, uniformPermutations(n))
    )
  }

  def maximalSwaps(n: Int): Gen[Permutation] = {
    val pairs = (0 to n-1).grouped(2).filter(_.size==2).toList
    assert(pairs.size == n/2, (n, n/2, pairs.size)) // n odd or even

    // the reverse permutation comprises n/2 swaps (0 n-1), (1 n-2), etc.
    // This is the maximal number of swaps and maximal number of cycles for any
    // permutation.
    val reverse = PermGroup(n).reverse

    // if n<=1 then reverse is also the identity. Otherwise
    // it is non-trivial, consisting of n/2 2-cycles, which we
    // assert.
    assert(n <= 1 || reverse.cycles.size == n/2)
    assert(n <= 1 || reverse.cycles.forall(_.size == 2))
    assert((n <= 1) == reverse.isIdentity)
    // generate random shufflings of the template by
    for {
      p <- uniformPermutations(n)
    } yield p * reverse * p.inverse // conjugate has same cyclic structure as template
  }

  /** Generator of uniformly distributed permutations. Not deterministic, that is not a function of Seed.
    *
    * @param n
    * @return
    */
  def uniformPermutations(n: Int): Gen[Permutation] =
    Gen.const((0 to n - 1).toVector).map(v => scala.util.Random.shuffle(v)).map(Permutation(_))

  /** An empty collection for the identity permutation.
    * Otherwise, for a permutation whose cyclic decomposition has n cycles, the result is size n.
    * Each element of the result is computed by reducing the length of each of the original cycles in turn.
    * */
  def shrinkCycle(c: Cycle): Cycle = if (c.size > 2) Cycle(c.rep.tail) else Cycle(Nil)

  def shrinkOnce(p: Permutation): Seq[Permutation] = {
    val cycles: Vector[Cycle] = p.cycles.filter(_.size > 0).toVector
    val shrunkenCycles = for (i <- 0 to cycles.size - 1) yield (cycles.take(i) ++: (shrinkCycle(cycles(i)) +: cycles.drop(i + 1)))
    val result = shrunkenCycles.map(cycles => Permutation.fromCycles(p.degree)(cycles))
    assert(result.size == result.distinct.size, s"expect reuslt to consist of unique values, $result")
    result
  }

  def shrinkTree(p: Permutation): Seq[Permutation] = {
    def loop(q: Permutation): Seq[Permutation] = {
      if (q.isIdentity) List(q)
      else {
        val result = for {r <- shrinkOnce(q)} yield r +: loop(r)
        q +: result.flatten.distinct
      }
    }

    val result = loop(p)
    result.filterNot(_ == p)
  }
}
