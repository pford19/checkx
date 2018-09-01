package pford19.checkx

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
// TODO transposition representation (not unique)
// TODO generate all permutations as a stream. Use algorithm in https://en.wikipedia.org/wiki/Permutation or
//
/**
  * A permutation of non-empty set., represented as a reordering of the sequence `(0 to n-1)`.
  * <p>
  * <h4>Degree</h4>
  * A permutation's `degree` is the size of the underlying set (i.e., the sequence `(0 to n-1)`) it acts on. Permutations acting on `(0 to n-1)`
  * have degree `n`.
  * <p>
  * <h4>Notations</h4>
  * Permutations can be represented several ways: as disjoint cycles, as a mapping, or as one-line implicit mapping.
  * <p>
  * For instance, the following are equivalent representations of a particular permutation on the sequence (0 to 5):
  * {{{
  *      explicit mapping: (0->1 1->2 2->0 3->4 4->3  5->5) // note that 5 maps to itself.
  *      cyclic:           (0 1 2)(3 4)
  *      implicit mapping: (1 2 0 4 3 5)
  * }}}
  * <p>
  * The explicit mapping can be read as follows. Each `i->j` pair in the mapping means the permutation
  * take the element at position `i` and moves it to position `j`.  The key set and value set are both (0 to n-1).
  * <p>
  * The implicit mapping is just the sequence of values of the explicit mapping in key order.
  * <p>
  * The cyclic representation consists of disjoint tuples. Each tuple of the form `(i j k ... m n)` is
  * equivalent to the explicit mappings `i->j`, `j->k`, ..., `m->n`, `n->i`. Note that the last element
  * of the tuple maps back to the first element, hence the name cycle.
  *
  * <p>
  * <h4>Identity</h4>
  * The simplest permutation does nothing. It is called the `identity` permutation.
  * <p>
  * For permutations of (0 to 5, the `identity` permutation has the following representations:
  * {{{
  * explicit mapping: (0->0 1->1 2->2 3->3 4->4 5->5)
  * cyclic:           ()
  * implicit mapping: (0 1 2 3 4 5)
  * }}}
  * <p>
  * <h4>Swaps</h4>
  * A swap is a permutation that exchanges exactly 2 elements. The cyclic representation is the most
  * concies for swaps.  `(0 3)` is the swap permutation that exchanges 0 and 3 and leaves all other elements fixed.
  * <p><h4>Cycles</h4>
  * Cycles are a particularly interesting subset of permutations. A cycle has a characteristic length, from 1 to n.
  * A 1-cycle does nothing; it maps x -> x, represented as `(x)`
  * Swaps are 2-cycles. The size of a cycle is the number of elements moved by the cycle.
  * <p>
  * 2-cycles are disjoint if they act on disjoint elements. Cycles (1 3 2) and (4 9 5) are disjoint, whereas
  * cycles (1 3 2) and (4 9 2) are not disjoint. In the later case, both cycles act on 2.
  * <p>
  * Disjoint cycles commute. Their overall effect is the regardless of the order they are applied. In general
  * this is not true if they are not disjoint.
  * <p>
  * Every permutation has a unique representation as a product of disjoint cycles. For instance, the example above
  * has a uni
  * {{{
  *      explicit mapping: (0->1 1->2 2->0 3->4 4->3  5->5) // note that 5 maps to itself.
  * }}}
  * has the unique cycle decomposition
  * {{{
  *      cyclic:           (0 1 2)(3 4)
  * }}}
  * Elements that are fixed by a permutation are typically not shown in the cyclic decomposition, but it is possible
  * to explicitly represent the fixed elements with 1-tuples. In the above example 5 is fixed, and could be represented
  * with an extended cyclic representation that include all the fixed elements.
  * {{{
  *      extended cyclic:  (0 1 2)(3 4)(5)
  * }}}
  *
  * <p>
  * <h4>Composition</h4>
  *
  * Permutations can be composed, or multiplied with the `*` operator.  The product is another permutation. The effect of
  * product permutation can be thought of as successive shuffles by the two terms.. As an example consider the following swaps.
  * {{{
  *   val p = Cycle(0 1)
  *   val q = Cycle(1 2)
  *   assert p*q = (0->2 1->0 2->1) = Cycle(0 2 1) // apply p first, then q
  *   assert q*p = (0->1 1->2 2->1) = Cycle(0 1 2) // apply q first, then p
  * }}}
  * Worth noting:
  * <ul>
  * <li> p*q != q*p, multiplication does not commute
  * <li> p and q are 2-cycles and their products are 3-cycles
  * </ul>
  * <p>
  * <h4>Inverse</h4>
  *
  * Permutation are invertible. For each permutation `p` there is an inverse permutation `p.inverse` that undoes the
  * action of `p`.
  * <p>
  * <h4>Laws</h4>
  *
  * There are several important "laws" about the identity, inverses, and multiplication.
  * <ul>
  * <li> (1) identity.inverse == identity
  * <li> (2) p*identity == identity*p = p for all p
  * <li> (3) p * p.inverse = p.inverse * p = identity
  * <li> (4) p.inverse.inverse = p
  * </ul>
  * <p>
  * In words, where p is an arbitrary permutation:
  * <ul>
  * <li> (1) The identity is its own inverse.
  * <li> (2) The identity commutes with p, and the product is p.
  * <li> (3) p and p.inverse commute and the product is the identity.
  * <li> (4) p inverted twice is p.
  * </ul>
  *
  * <p>
  *
  **/

/**
  *
  * @param rep permutation of (0 to n-1) for some integer n
  */

case class Permutation(rep: Seq[Int]) {

  require(rep.toSet == (0 to rep.size - 1).toSet)

  /** The number of elements in the set permuted by this permutation. */
  val degree = rep.size

  /** The identity permutation of the same degree as `this`.
    */
  def identity: Permutation = Permutation.identity(degree)

  /** Is `this` the identity permutation? */
  def isIdentity: Boolean = this == Permutation.identity(degree)

  /** The inverse of `this`. */
  def inverse: Permutation = powers.last

  /** The product of this and other. In general, not commutative. That is, `this*other`
    * and `other*this` may not be equal.
    *
    * It this and other aren't the same degree, the result has the maximum degree of the two.
    *
    * @param other permutation
    * @return
    */
  def *(other: Permutation): Permutation = {

    if (degree != other.degree) {
      val x = Math.max(degree, other.degree)
      val p = this.extend(x)
      val q = other.extend(x)
      p * q
    } else {
      val buff = new ArrayBuffer[Int]
      for (i <- 0 to rep.size - 1) {
        buff += rep(other.rep(i))
      }
      Permutation(buff.toVector)
    }
  }

  /** Extend to a permutation on `m` elements. */
  def extend(m: Int): Permutation = {
    require(m >= degree)
    if (m > degree) {
      val buff = new ArrayBuffer[Int]()
      buff ++= rep
      buff ++= (degree to m - 1)
      Permutation(buff.toVector)
    } else
      this
  }

  /** Contract, if possible, to a permutation on `m` elements. */
  def contract(m: Int): Permutation = {
    require(m >= 0)
    if (m < degree) {
      require(rep.drop(m) == (m to degree - 1))
      Permutation(rep.take(m))
    } else
      this
  }

  /** Fixed points under this permutation.
    *
    * @return Elements of (0 to n-1) that are fixed under this permutation.
    */
  def fixedPoints: Set[Int] = rep.zip(0 to degree - 1).filter { case (p, j) => p == j }.map(_._1).toSet

  /** Elements of (0 to n-1) that are not fixed under this permutation.
    *
    * @return
    */
  def nonFixedPoints: Set[Int] = rep.toSet.diff(fixedPoints)

  /** `p`^n^, the n'th power, that is, the product of p with itself n times.
    *
    * @param n power, may be be zero, positive or negative.
    * @return
    */
  def ^(n: Int): Permutation = {
    val j = n % order;
    if (j < 0) powers(order + j) else powers(j)
  }


  /** The orbit q, p*q, p^2^*q, p^3^*q, ...` (where `p` is this permutation).
    *
    * @param q
    * @return
    * */

  def leftOrbit(q: Permutation): Seq[Permutation] = powers.map(_ * q)

  /** The orbit `q, q*p, q*p^2^, q*p^3^, ...` (where `p` is this permutation).
    *
    * @param q
    * @return
    */
  def rightOrbit(q: Permutation): Seq[Permutation] = powers.map(q * _)

  /** The sequence: Seq(identity, this, this^2^, this^3^, ..., this^n^) where `n+1` is the order of this permutation.
    * Lazy, so not computed until needed, but then retained. Note that the first two elements could be written `this`^0^
    * and `this`^1^.
    */
  lazy val powers: Stream[Permutation] = {

    def rest(p: Permutation): Stream[Permutation] = {
      if (p == identity) Stream.empty
      else Stream.cons(p, rest(this * p))
    }

    Stream.cons(identity, rest(this))
  }

  /** The order of this permutation. Order is the smallest positive integer n such that `this^n^ == identity`.
    */
  def order: Int = powers.size

  /** The orbit of element `e`. `e` is an element of `(0 to degree-1)`.
    * If this is the permutation p, then the orbit of e is the sequence (e, p(e), p(p(e)), p(p(p(e), ...).
    * It terminates when p^n^(e) == e, which will always happen.
    * For example consider `p`
    * {{{
    *   p = Permutation(List(1 0 2 3))
    *   p.cyclicRepresentation == (0 1)(2)(3)
    * }}}
    * Then {{{
    *   p.orbit(0) == p.orbit(1) == (0 1)
    *   p.orbit(2) == (2)
    *   p.orbit(3) == (3)
    * }}}
    * As the example shows, for permutation p, the orbit of the elements of (0 to degree-1) are directly connected with the
    * cyclic representation of p.
    *
    * @param e element of (0 to degree-1)
    * @return
    */
  def orbit(e: Int): Cycle = {

    @tailrec def loop(x: Int, orb: Seq[Int], i: Int): Seq[Int] = {
      if (i > degree)
        throw new IllegalStateException(s"orbit size should be less than n for $rep, but is $orb")
      else if (x == e)
        orb.reverse
      else
        loop(rep(x), x +: orb, i + 1)
    }

    val result = loop(rep(e), List(e), 0)
    Cycle(result).normalized
  }

  /** Cyclic decomposition into disjoint cycles.
    * Each component cycle in the result is normalized.
    * Singleton cycles are omitted.
    * The identity permutation is represented by the List(Cycle(Nil)).
    *
    * @return normalized and sorted by head element, never empty.
    */

  def cycles: Seq[Cycle] = {
    if (isIdentity)
      List(Cycle(Nil))
    else
      (for (i <- (0 to degree - 1))
        yield orbit(i)) // orbit: Cycle
        .filter(c => c.size > 1) // exclude singleton orbits
        .map(c => c.normalized) // normalize cycles (min element first)
        .sortBy(c => c.rep.head) // sorted by first element of normalized cycles
        .distinct
  }

  /** Cyclic decomposition including singletons. */
  def fullcycles: Seq[Cycle] = {
      (for (i <- (0 to degree - 1))
        yield orbit(i)) // orbit: Cycle
        .map(c => c.normalized) // normalize cycles (min element first)
        .sortBy(c => c.rep.head) // sorted by first element of normalized cycles
        .distinct
  }

  /** Is this permutation a cycle?
    *
    * @return
    */

  def isCycle: Boolean = {
    val points = nonFixedPoints
    points.size == 0 || orbit(points.head).size == points.size
  }

  override def toString = "Perm" + fullCyclicRepresentation

  def cyclicRepresentation: String = {
    val cs = cycles
    "(" + cs.map { c => "(" + c.rep.mkString(" ") + ")" }.mkString("") + ")"

  }
  def fullCyclicRepresentation: String = {
    val cs = fullcycles
    "(" + cs.map { c => "(" + c.rep.mkString(" ") + ")" }.mkString("") + ")"

  }

  def mapRepresentation: String = {
    val pairs = (0 to degree - 1).zip(rep)
    val stringpairs = pairs.map(p => s"${p._1}->${p._2}").mkString(" ")
    "(" + stringpairs + ")"
  }
}

case class PermutationOps(wrapped: Permutation) {
  def actOn[T](seq: Seq[T]): Seq[T] = {
    require(seq.size == wrapped.degree)
    val result = seq.zip(wrapped.rep).sortBy(_._2).unzip._1
    result
  }
}


object Permutation {
  def apply(xs: Seq[Int]): Permutation = new Permutation(xs)

  def apply(n: Int, more: Int*): Permutation = new Permutation(n +: more)

  def identity(n: Int): Permutation = new Permutation((0 to n - 1).toVector)

  def fromCycles(n: Int)(cycles: Iterable[Cycle]): Permutation = {
    cycles.map(_.asPermutation(n)).foldLeft(identity(n))(_ * _)
  }



}



object PermutationOps {
  implicit def toPermutationOps(wrapped: Permutation): PermutationOps = PermutationOps(wrapped)
}

case class Cycle(rep: Seq[Int]) {

  require(rep.isEmpty || rep.min >= 0)
  require(rep.toSet.size == rep.size)

  def size: Int = rep.size

  def normalized: Cycle = {
    rotate(rep.zipWithIndex.min._2)
  }

  /** Permutation that "rotates" `i` to `(i + n) % size`.
    *
    * @param n positive, negative, or zero
    * @return
    */
  def rotate(n: Int): Cycle = {
    if (n == 0) this
    if (n < 0) rotate(n - n % size)
    else {
      val (a, b) = rep.splitAt(n % size)
      Cycle(b ++: a)
    }
  }

  /** This cycle as a permutation of n elements.
    *
    * Example
    * {{{
    *   Cycle(0, 1) ==> Permutation(1, 0, 2, 3, 4, 5, ...)
    *   Cycle(3, 2, 4) ==> Permutation(0, 1, 4, 2, 3, 5, ...)
    * }}}
    *
    * @param n
    * @return
    */
  def asPermutation(n: Int): Permutation = {
    require(rep.isEmpty || rep.max < n)
    val buff = new ArrayBuffer[Int]
    for (i <- 0 to n - 1) buff += i // buff is the identity at this point
    if (rep.nonEmpty) {
      val extended = rep.last +: rep
      val pairs = extended.zip(rep)
      assert(pairs.size == rep.size)
      for ((from, to) <- pairs) buff(from) = to
    }
    Permutation(buff.toVector)
  }

  override def toString: String = "Cycle(" + rep.map(_.toString).mkString(" ") + ")"

}

object Cycle {
  def apply(n: Int, more: Int*): Cycle = Cycle(n +: more)
  implicit def cycleToPermutation(c: Cycle): Permutation = c.asPermutation(c.rep.max+1)
}


/** Finite permutation group on `n` elements.
  *
  * @param degree
  */
case class PermGroup(degree: Int) {
  val rep = (0 to degree - 1).toVector
  val elements = rep

  val identity: Permutation = Permutation(elements)

  /** Transposition (swap) of `j` and `k`. Permutation with cyclic representation `(j k)`.
    *
    * @param j
    * @param k
    * @return
    */
  def transpose(j: Int, k: Int): Permutation = {
    val buff = new ArrayBuffer[Int]
    buff ++= rep
    val t = buff(j)
    buff(j) = buff(k)
    buff(k) = t
    Permutation(buff.toVector)
  }

  /** Permutation that maps each `i` to `i+1 % n`.
    *
    * @return
    */
  def rotation: Permutation = degree match {
    case 0 => identity
    case 1 => identity
    case _ => Permutation(elements.tail :+ elements.head)
  }

  def reverse: Permutation = {
    @tailrec def swapFromEnds(j: Int, k: Int, p: Permutation): Permutation =
      if (j >= k) p else swapFromEnds(j + 1, k - 1, p * transpose(j, k))

    swapFromEnds(0, degree - 1, identity)

  }

  def rotations: Seq[Permutation] = rotation.powers

  def reverseRotations: Seq[Permutation] = (reverse * rotation).powers

  def transpositions: Seq[Permutation] = for (i <- 0 to degree - 2; j <- i + 1 to degree - 1) yield transpose(i, j)


}



