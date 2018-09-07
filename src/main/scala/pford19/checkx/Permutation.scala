package pford19.checkx

import pford19.checkx

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Pmaps are "permutation maps".  They are represented as minimal `Int` to `Int` automorphism maps.
  * A map `m` is an automorphism if and only if `m.keySet == m.values.toSet`.
  * An automorpishm map `m` is minimal if it has no fixed points, that is {{{
  *   m(j) != j for all j in m.keySet
  * }}}
  *
  * @param rep representation mapping, an automorphism (key set == value set) with no fixed points
  */

case class Pmap private(rep: Map[Int, Int]) {

  require(rep.keySet == rep.values.toSet, s"mapping should be an automorphism $rep")
  require(rep.forall { case (k, v) => k != v }, s"mapping should have no fixed points $rep")


  /** Minimum degree of a permutation with this `Pmap`. */
  val minDegree = if (rep.size == 0) 0 else rep.keys.max + 1
  /** The size of this Pmap, the number of non-fixed points. */
  val size = rep.size

  /** Inverse Pmap.
    *
    * @return
    */
  def inverse = new Pmap(rep.map { case (i, j) => (j, i) })

  /** Mapping of `n`.
    *
    * @param n arbitrary integer
    * @return
    */
  def apply(n: Int) = rep.getOrElse(n, n)

  /** Composition of Pmaps as a composition of automorphisms.
    *
    * @param other arbitrary Pmap
    * @return product Pmap
    */
  def *(other: Pmap) = {
    val mapping = for {
      i <- rep.keySet.union(other.rep.keySet)
      j = apply(other(i))
    } yield (i -> j)
    Pmap(mapping.toMap)
  }

  def orbit(n: Int): Seq[Int] = {
    val result = new ArrayBuffer[Int]()
    result += n
    var k = this (n)
    while (k != n) {
      result += k
      k = this (k)
    }
    result.toVector
  }
}

/**
  * Companion object providing
  * <ul>
  * <li>
  * a convenience constructor that accepts a non-minimal automorphism
  * <li>
  * a singleton `identity` val, the empty `Pmap`.
  * </ul>
  */
object Pmap {
  /** Pmap based on `mapping`. `mapping` is filtered to remove fixed points before invoking Pmap constructor.
    *
    * @param mapping arbitrary automorphism, possibly with fixed points
    * @return
    */
  def apply(mapping: Map[Int, Int]): Pmap = new Pmap(mapping.filter { case (k, v) => k != v })

  val identity: Pmap = Pmap(Map.empty)
}


/** Abstract permutation implemented by concrete [[Identity]], [[Cycle]], and [[Permutation]].
  * All implementations are based on a `Pmap`
  * <p>
  * This trait provides final implementation for operations defined solely in terms of the `pmap`: `equals`, `hahshCode`, 
  * `identity`, `minDegree`, `nonFixedPoints`, `^`, `apply`, `actingOn`, `cyclicRepresentation`, `fullCyclicRepresentation`,
  * `mapRepresentation`
  * <p>
  * Default implementations are provided for: `isIdentity`, `isCycle`, `isTransposition`, `orbit`, `toString`, 
  * <p> 
  * Abstract elements include: `pmap`, `degree`, `order`, `*`, `cycles`, `fullcycles`, `powers`.
  *
  */
trait Perm {

  def pmap: Pmap

  final override def equals(obj: scala.Any): Boolean = {
    val result = obj match {
      case p: Perm => (this eq p) || (this.pmap == p.pmap)
      case _ => false
    }
    result
  }

  /** @note OMG! Newbie error. Implmented equals but forgot hashCode. Saved by unit tests. 6-sep-2018 pford
    * @return
    */
  final override def hashCode(): Int = pmap.hashCode()

  final val identity: Perm = Identity

  final lazy val minDegree: Int = pmap.minDegree

  def degree: Int

  def inverse: Perm

  def order: Int

  def fixedPoints: Set[Int]

  final def nonFixedPoints: Set[Int] = pmap.rep.keySet

  def *(other: Perm): Perm

  /** `p`^n^, the n'th power, that is, the product of p with itself n times.
    *
    * @param n power, may be be zero, positive or negative.
    * @return
    */
  final def ^(n: Int): Perm = {
    val j = n % order;
    if (j < 0) powers(order + j) else powers(j)
  }


  def isIdentity: Boolean = pmap.size == 0

  def isCycle: Boolean = if (pmap.size == 0) true else {
    pmap.orbit(pmap.rep.keys.head).size == pmap.size
  }

  def isTransposition: Boolean = pmap.size == 2

  def orbit(j: Int): Cycle = Cycle(pmap.orbit(j))

  def cycles: Seq[Cycle]

  def fullcycles: Seq[Cycle]


  /** Apply `this` to `seq`. `seq` must be at least length `degree`.
    * Result is new sequence reordered by the `this` permutation.
    * Any elements of `seq` at an index >= `degree` are left fixed.
    *
    * @param seq arbitrary sequence of length at least `degree`
    * @tparam T element type of `seq`
    * @return permuted copy of `seq`
    */
  final def actingOn[T](seq: Seq[T]): Seq[T] =
    seq.zipWithIndex.map { case (s, i) => (s, this.pmap(i)) }.sortBy(_._2).map(_._1)

  def powers: Seq[Perm]

  final def apply(n: Int): Int = pmap(n)


  override def toString = "Perm" + fullCyclicRepresentation


  /** String representation as cycles, excluding 1-cycles for fixed points.
    * Each cycle is normalized (minimum element first)
    * and the cycles are ordered by their first element.
    *
    * @return cyclic representation excluding fixed points
    */
  final def cyclicRepresentation: String = {
    val cs = cycles
    "(" + cs.map {
      c => "(" + c.rep.mkString(" ") + ")"
    }.mkString("") + ")"

  }

  /** Cyclic representation including 1-cycles for fixed points.
    * Each cycle is normalized (minimum element first)
    * and the cycles are ordered by their first element.
    *
    * @return cyclic representation including 1-cycles for fixed points.
    */
  final def fullCyclicRepresentation: String = {
    val cs = fullcycles
    "(" + cs.map {
      c => "(" + c.rep.mkString(" ") + ")"
    }.mkString("") + ")"

  }

  /** Representation as a list of map pairs (`i->j`).
    *
    * @return representation as a list of map pairs
    */
  final def mapRepresentation: String = {
    val pairs = pmap.rep.toList.sortBy(_._1)
    val stringpairs = pairs.map(p => s"${
      p._1
    }->${
      p._2
    }").mkString(" ")
    "(" + stringpairs + ")"
  }


}

/**
  * Companion object.
  * 
  */
object Perm {

  /** Representation of `p` as a sequence (product) of basic transpositions.
    * <p>
    * A basic transposition is of the form `(j, j+1)`. It swaps adjacen integer elements.
    * <p>
    * The steps are
    * <ol>
    * <li>Represent `p` as a product of disjoint cycles.
    * <li>Represent each cycle as a product of transpositions.
    * <li>Represent each transposition as a product of basic transpositions.
    * </ol>
    * <p>
    * Example {{{
    *                       Permutation(0 3 4 5 2 1) =>
    * cycles:               (135)(24) =>
    * transpositions:       (13)(35) (24) =>
    * basic transpositions: (12)(23)(12) (34)(45)(34) (23)(34)(23)
    * }}}
    *
    * @param p Arbitrary permutation
    * @return sequence of basic transpositions whose product is `p`
    */
  def basicTranspositions(p: Perm): Seq[Transposition] = {
    val cycles = p.cycles
    val trans = cycles.flatMap(asTranspositions)
    val basics = trans.flatMap(asBasicTranspositions)
    basics
  }

  /** Representation of a transposition `t` as a sequence of basic transpositions.
    * <p>
    *   Example: {{{
    *     (25) == (23)(34(45)(34)(23)
    *   }}}
    * @param t
    * @return
    */
  def asBasicTranspositions(t: Transposition): Seq[Transposition] = {
    val result = if (t.j == t.k - 1)
      List(t)
    else {
      val up = (t.j to t.k - 1)
      val ups = up.zip(up.tail).map(Transposition(_))
      ups ++ (Transposition(t.k - 1, t.k) +: ups.reverse)
    }

    assert(result.forall(_.isBasic))

    assert(result.foldLeft(PermGroup(1).identity) { (a: Perm, b: Perm) => a * b } == t.asPermutation)
    result
  }

  def asTranspositions(c: Cycle): Seq[Transposition] = {
    c.rep.zip(c.rep.tail).map(Transposition(_))
  }
}


case object Identity extends Perm {
  val pmap: Pmap = Pmap.identity

  val degree: Int = 0

  val inverse: Perm = this

  override val order: Int = 1

  val fixedPoints: Set[Int] = Set.empty

 // val nonFixedPoints: Set[Int] = Set.empty

  def *(other: Perm): Perm = other

  override val isIdentity: Boolean = true

  override val isCycle: Boolean = true

  override val isTransposition: Boolean = false

  override def orbit(j: Int): Cycle = Cycle(j)

  val cycles: Seq[Cycle] = Nil

  val fullcycles: Seq[Cycle] = Nil

  val powers: Seq[Perm] = List(this)

}


class Transposition(val rep: (Int, Int)) extends Perm {
  val j = Math.min(rep._1, rep._2)
  val k = Math.max(rep._2, rep._1)
  require(0 <= j)
  require(j < k, (j, k))

  val degree: Int = k + 1

  val inverse: Perm = this

  override val order: Int = 2

  lazy val fixedPoints: Set[Int] = (0 to k - 1).toSet.diff(Set(j))

  //lazy val nonFixedPoints: Set[Int] = Set(j, k)

  def *(other: Perm): Perm = {
    val p = pmap * other.pmap
    Permutation(p.minDegree, p)
  }

  override val isIdentity: Boolean = false

  override val isCycle: Boolean = true

  val pmap: Pmap = Pmap(Map(j -> k, k -> j))

  def isBasic = j + 1 == k

  override def isTransposition: Boolean = true

  override def orbit(n: Int): Cycle = {
    if (n != j && n != k) Cycle(n)
    else Cycle(j, k)
  }

  def cycles: Seq[Cycle] = List(Cycle(j, k))

  def fullcycles: Seq[Cycle] = (0 to j - 1).map(Cycle(_)) ++ (Cycle(j, k) +: (j + 1 to k - 1).map(Cycle(_)))

  def asPermutation: Permutation = Permutation(degree, pmap)

  def powers: Seq[Perm] = List(identity, this)

}

object Transposition {
  def apply(pair: (Int, Int)): Transposition = {
    new Transposition(pair)
  }

  def apply(j: Int, k: Int): Transposition = {
    new Transposition((j, k))
  }
}

/**
  * A permutation of non-empty set, of degree n`, represented as a reordering of the sequence `(0 to n-1)`.
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
  *      minimal cyclic:   (0 1 2)(3 4)
  *      full cyclic:      (0 1 2)(3 4)(5)
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
  * of the tuple maps back to the first element, hence the name cycle. The minimal cyclic notation
  * omits the 1-cycles for fixed elements. The full cyclic notation include a 1-cycle for each fixed element.
  *
  * <p>
  * <h4>Identity</h4>
  * The simplest permutation does nothing. It is called the `identity` permutation.
  * <p>
  * For permutations of (0 to 5, the `identity` permutation has the following representations:
  * {{{
  * explicit mapping: (0->0 1->1 2->2 3->3 4->4 5->5)
  * minimal cyclic:   ()
  * full cyclic:      (0)(1)(2)(3)(4)(5)
  * implicit mapping: (0 1 2 3 4 5)
  * }}}
  * <p>
  * <h4>Transposition</h4>
  * A transposition is a permutation that exchanges exactly 2 elements. The cyclic representation is the most
  * concise for transpositions.  For example, `(0 3)` is the permutation that transposes 0 and 3 and leaves all other elements fixed.
  * <p><h4>Cycles</h4>
  * Cycles are a particularly interesting subset of permutations. A cycle has a characteristic length, from 1 to n.
  * A 1-cycle does nothing; it maps x -> x, represented as `(x)`
  * Swaps are 2-cycles. The size of a cycle is the number of elements moved by the cycle.
  * <p>
  * Cycles are disjoint if they act on disjoint elements. Cycles (1 3 2) and (4 9 5) are disjoint, whereas
  * cycles (1 3 2) and (4 9 2) are not disjoint. In the later case, both cycles act on 2.
  * <p>
  * Disjoint cycles commute. Their overall effect is the regardless of the order they are applied. In general
  * this is not true if they are not disjoint.
  * <p>
  * Every permutation has a unique representation as a product of disjoint cycles (that is, unique up to ordering).
  * {{{
  *      explicit mapping: (0->1 1->2 2->0 3->4 4->3  5->5) // note that 5 maps to itself.
  * }}}
  * has the unique cycle decomposition
  * {{{
  *      cyclic:           (0 1 2)(3 4)
  * }}}
  * Elements that are fixed by a permutation are typically not shown in the cyclic decomposition, but it is possible
  * to explicitly represent the fixed elements with 1-tuples. In the above example 5 is fixed, and could be represented
  * with an full cyclic representation that include all the fixed elements.
  * {{{
  *      full cyclic:  (0 1 2)(3 4)(5)
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

/** General permutation acting on `(0 to n-1)` where `n = rep.size`, potentially with multiple cycles.
  * <p>
  * The elements of `rep` must be the set `(0 to n-1)`.
  * Example {{{
  *   val p = Permutation(1, 0, 2, 5, 3, 4)
  * }}}
  * has the full cyclic representation {{{
  *   (0 1)(2)(3 5 4))
  * }}}
  * the two-line representation {{{
  *   0, 1, 2, 3, 4, 5
  *   1, 0, 2, 5, 3, 4
  * }}} and the mapping representation {{{
  *   0->1, 1->0, 2->2, 3->5, 4->3, 5->4
  * }}}
  * Applying this example permutation p to the the list `(0, 1, 2, 3, 4, 5)` yields the shuffled list `(1, 0, 2, 4, 5, 3)`. {{{
  *   p.applyingTo(List(0,1,2,3,4,5)) => List(1,0,2,4,5,3)
  * }}}
  *
  * A `Permutation` instance can represent the identity permutation, a cycle, or a transposition. These more specialized 
  * types of permutations also have their own classes with more concise constructors. {{{
  *   Identity == Permutation(0)
  *   Transposition(2, 4) == Permutation(0, 1, 4, 3, 2)
  *   Cycle(1, 3, 5) == Permutation(0, 3, 2, 5, 4, 1)
  * }}}
  *
  * @param rep One-line representation, the value `j` in the `i'th` position indicates that `i` is mapped to `j`.
  */

class Permutation(val rep: Seq[Int]) extends Perm {

  require(rep.toSet == (0 to rep.size - 1).toSet)

  val pmap = Pmap((0 to rep.size - 1).zip(rep).toMap)

  /** The number of elements in the set permuted by this permutation. */
  val degree = rep.size


  //  /** Is `this` the identity permutation? */
  //  val isIdentity: Boolean = (pmap.rep.size == 0)

  /** The inverse of `this`. */
  def inverse: Permutation = Permutation(degree, pmap.inverse)


  /** The order of this permutation. Order is the smallest positive integer n such that `this^n^ == identity`.
    */
  lazy val order: Int = {
    val p = powers
    p.size
  } // lazy to avoid recursion


  /** The product of `this` and `other`. In general, not commutative. That is, `this*other`
    * and `other*this` may not be equal.
    *
    * It `this` and `other` aren't the same degree, the result has the maximum degree of the two.
    *
    * @param other permutation
    * @return
    */
  def *(other: Perm): Perm = Permutation(Math.max(degree, other.degree), pmap * other.pmap)


  /** Ensure that `this` is a permutation on at least `m` elements.
    * If `this.degree >= m`, then the result is `this`.
    * If `this.degree < m`, then the result is a new Permuation backed by the same `Pmap` with degree `m`.
    *
    * @param m required degree
    * @return Permutation with `degree == Math.max(this.degree,m)` backed by the same `Pmap` as `this`.
    */
  def extend(m: Int): Permutation = {
    if (m > degree) {
      Permutation(m, pmap)
    } else
      this
  }

  /** Contract, if possible, to a permutation on `m` elements. */
  def contract(m: Int): Permutation = {
    require(m >= pmap.minDegree)
    if (m < degree) {
      Permutation(m, pmap)
    } else
      this
  }

  /** Fixed points under this permutation.
    *
    * @return Elements of (0 to n-1) that are fixed under this permutation.
    */
  def fixedPoints: Set[Int] = (0 to degree - 1).toSet.diff(pmap.rep.keySet)

  /** Elements of (0 to n-1) that are not fixed under this permutation.
    *
    * @return
    */
  //def nonFixedPoints: Set[Int] = pmap.rep.keySet


  //  /** The orbit q, p*q, p^2^*q, p^3^*q, ...` (where `p` is this permutation).
  //    *
  //    * @param q
  //    * @return
  //    * */
  //
  //  def leftOrbit(q: Perm): Seq[Perm] = powers.map(_ * q)
  //
  //  /** The orbit `q, q*p, q*p^2^, q*p^3^, ...` (where `p` is this permutation).
  //    *
  //    * @param q
  //    * @return
  //    */
  //  def rightOrbit(q: Perm): Seq[Perm] = powers.map(q * _)

  /** The sequence: `Seq`(identity, this, this^2^, this^3^, ..., this^n-1^) where `n` is the order of this permutation.
    * Note that the first two elements could be written `this`^0^
    * and `this`^1^.
    */
  def powers: Seq[Perm] = {

    def rest(p: Pmap): Stream[Pmap] = {
      p match {
        case Pmap.identity => Stream.empty
        case _ => Stream.cons(p, rest(p * this.pmap))
      }
    }

    val s = Pmap.identity +: rest(this.pmap)
    val result = s.map(Permutation(degree, _)).toVector
    result

  }

  /** The orbit of element `e`. `e` is an element of `(0 to degree-1)`.
    * If this is the permutation `p`, then the orbit of e is the sequence `(e, p(e), p(p(e)), p(p(p(e), ...)`.
    * It terminates when `p`^n^`(e) == e` for some `n` (which is guaranteed, since `this` is a finite permutation.
    * For example consider
    * {{{
    *   p = Permutation(List(1 0 2 3))
    *   p.cyclicRepresentation == (0 1)(2)(3)
    * }}}
    * Then {{{
    *   p.orbit(0) == p.orbit(1) == (0 1)
    *   p.orbit(2) == (2)
    *   p.orbit(3) == (3)
    * }}}
    * As the example shows, for permutation `p`, the orbit of the elements of (0 to degree-1) are directly connected with the
    * cyclic representation of p.
    *
    * @param e element of (0 to degree-1)
    * @return
    */
  //  def orbit(e: Int): Cycle = {
  //
  //    @tailrec def loop(x: Int, orb: Seq[Int], i: Int): Seq[Int] = {
  //      if (i > degree)
  //        throw new IllegalStateException(s"orbit size should be less than n for $rep, but is $orb")
  //      else if (x == e)
  //        orb.reverse
  //      else
  //        loop(rep(x), x +: orb, i + 1)
  //    }
  //
  //    val result = loop(rep(e), List(e), 0)
  //    Cycle(result).normalized
  //  }

  /** Cyclic decomposition into disjoint cycles, excluding fixed-point 1-cycles.
    * Each component cycle in the result is normalized.
    * The identity permutation is represented by the List(Cycle(Nil)).
    *
    * @return normalized and sorted by head element, never empty.
    */

  def cycles: Seq[Cycle] = {
    val r1 = if (isIdentity)
      List(Cycle(Nil))
    else
      (for (i <- (0 to degree - 1))
        yield orbit(i)) // orbit: Cycle

    val r2 = r1.filter(c => c.size > 1) // exclude singleton orbits
    val r3 = r2.map(c => c.normalized) // normalize cycles (min element first)
    val r4 = r3.sortBy(c => c.rep.head) // sorted by first element of normalized cycles
    val r5 = r4.distinct
    r5
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
    * @return true if a cycle (including the identity), false otherwise
    */

  //  def isCycle: Boolean = {
  //    val points = nonFixedPoints
  //    points.size == 0 || orbit(points.head).size == points.size
  //  }

  /** This `Permutation` instance as a `Cycle` instance, if it is, in fact, a cycle.
    * Otherwise throws a `RuntimeException`.
    *
    * @return equivalent cycle object
    */

  def asCycle: Cycle = {
    require(isCycle)
    if (isIdentity) Cycle(Nil)
    else orbit(nonFixedPoints.head)
  }


  //  def isTransposition: Boolean = isCycle && order == 2

  def asTransposition: Transposition = {
    require(isTransposition)
    Transposition(pmap.rep.head)
  }
}

/** Companion object. Provides factory functons and an `identity` function.
  *
  */
object Permutation {
  /** Permutation from 1-line representation.
    *
    * @param xs
    * @return
    */
  def apply(xs: Seq[Int]): Permutation = new Permutation(xs)

  /** Permutation of degree `degree` from a `pmap`.
    *
    * @param degree
    * @param pmap
    * @return
    */
  def apply(degree: Int, pmap: Pmap): Permutation = {
    require(pmap.minDegree <= degree, (pmap, degree))
    Permutation(
      (0 to degree - 1).map(pmap(_))
    )
  }

  /** Permutation from 1-line representation.
    *
    * @param n
    * @param more
    * @return
    */
  def apply(n: Int, more: Int*): Permutation = new Permutation(n +: more)

  /** Identity permutatio on `n` elements.
    *
    * @param n
    * @return
    */
  def identity(n: Int): Perm = Cycle(Nil)

  /** Permutation of degree `n` constructed from `cycles`.
    *
    * @param degree degree of the permutation
    * @param cycles cycles, not necessarily disjoint
    * @return permutation that is the product of the cycles
    */
  def fromCycles(degree: Int)(cycles: Iterable[Cycle]): Perm = {
    cycles.foldLeft(identity(degree))((a: Perm, b: Perm) => a * b)
  }


}

/**
  *
  * @param rep cyclic representation
  */
class Cycle(val rep: Seq[Int]) extends Perm {

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

  /** This cycle as a permutation of `n` elements.
    *
    * Example
    * {{{
    *   Cycle(0, 1) ==> Permutation(1, 0, 2, 3, 4, 5, ...)
    *   Cycle(3, 2, 4) ==> Permutation(0, 1, 4, 2, 3, 5, ...)
    * }}}
    *
    * @param n degree of the result
    * @return equivalent Permutation
    */
  def asPermutation(n: Int): Permutation = {
    require(rep.isEmpty || rep.max < n)
    Permutation(n, pmap)
  }

  /** Pmap for this cycle.
    *
    * @return
    */
  val pmap: Pmap = if (rep.isEmpty) Pmap.identity else Pmap(rep.zip(rep.tail :+ rep.head).toMap)


  override def toString: String = "Cycle(" + rep.map(_.toString).mkString(" ") + ")"

  val degree: Int = if (rep.isEmpty) 0 else rep.max + 1

  def inverse: Cycle = Cycle(rep.reverse)

  val order: Int = size

  def fixedPoints: Set[Int] = (0 to degree - 1).toSet.diff(rep.toSet)

  //def nonFixedPoints: Set[Int] = rep.toSet

  def *(other: Perm): Perm = {
    val d = Math.max(other.degree, degree)
    Permutation(d, this.pmap * other.pmap)
  }

  //  val isIdentity: Boolean = size == 0

  override val isCycle: Boolean = true

  override def isTransposition: Boolean = order == 2

  override def orbit(j: Int): Cycle =
    if (rep.contains(j)) this
    else Cycle(j)

  def cycles: Seq[Cycle] = List(this)

  def fullcycles = {
    (this +: fixedPoints.map(Cycle(_)).toList).sortBy(_.rep.head)
  }

  def asPermutation: Permutation = Permutation(degree, pmap)

  def powers: Seq[Perm] = {
    // Recall that size of scanLeft is input size + 1.
    // Specifically powers for a two cycle (order 2) is generated as
    // Nil.scanLeft(identity){ (p,n) => p*this } == List(identity, this)
    val result = (0 to order - 2).scanLeft(identity)((p: Perm, n: Int) => p * this)
    result
  }
}

object Cycle {
  def apply(rep: Seq[Int]): Cycle = new Cycle(rep)

  def apply(n: Int, more: Int*): Cycle = new Cycle(n +: more)

}


/** Finite permutation group on `n` elements.
  *
  * @param degree
  */
case class PermGroup(degree: Int) {
  val rep = (0 to degree - 1).toVector
  val elements = rep

  val identity: Perm = Identity

  /** Transposition (swap) of `j` and `k`. Permutation with cyclic representation `(j k)`.
    *
    * @param j
    * @param k
    * @return
    */
  def transpose(j: Int, k: Int): Transposition = {
    require(0 <= j)
    require(0 <= k)
    require(j < degree)
    require(k < degree)
    //    val m = Map(j -> k, k -> j)
    //    val p = Pmap(m)
    Transposition(j, k)
  }

  /** Permutation that maps each `i` to `i+1 % n`.
    *
    * @return
    */
  def rotation: Perm = degree match {
    case 0 => identity
    case 1 => identity
    case _ => Permutation(elements.tail :+ elements.head)
  }

  def reverse: Perm = {

    @tailrec def swapFromEnds(j: Int, k: Int, p: Perm): Perm =
      if (j >= k) p else swapFromEnds(j + 1, k - 1, p * transpose(j, k))

    swapFromEnds(0, degree - 1, identity)

  }

  def rotations: Seq[Perm] = rotation.powers.toVector

  def reverseRotations: Seq[Perm] = {
    val p = (reverse * rotation)
    p.powers.toVector
  }

  /** All transpositions of this `PermGroup`.
    *
    * @return
    */
  def transpositions: Seq[Perm] = for (i <- 0 to degree - 2; j <- i + 1 to degree - 1) yield transpose(i, j)


}

