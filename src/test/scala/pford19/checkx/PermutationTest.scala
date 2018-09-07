package pford19.checkx

import org.scalatest.{FlatSpec, MustMatchers}

/** Test permutations: Perm, Permutation, Transposition, Cycle, Identity, PermGroup.
  *
  */

class PermutationTest extends FlatSpec with MustMatchers {

  val pg5 = PermGroup(5)
  val swap01 = Permutation(1, 0, 2, 3, 4)
  val cycle01 = Cycle(0, 1)
  val cycle10 = Cycle(1, 0)
  val cycle5 = Cycle(0, 1, 2, 3, 4)
  val reverse5 = Permutation(4, 3, 2, 1, 0)
  val cycle04 = Cycle(0, 4)
  val cycle13 = Cycle(1, 3)
  val cycle123 = Cycle(1, 2, 3)


  "Perm.equals" must "work for identity" in {
    assert(Identity == Identity)
    assert(Identity == Cycle(Nil))
    assert(Identity == Permutation(0, 1, 2))

    assert(Identity != Cycle(0, 1))
    assert(Identity != Transposition(0, 1))
    assert(Identity != Permutation(1, 0, 2))
  }

  it must "work for cycles" in {
    val c = Cycle(0, 1, 2)
    assert(c == c)
    assert(c == Cycle(0, 1, 2))
    assert(c == Cycle(1, 2, 0))
    assert(c == Cycle(2, 0, 1))
    assert(c == Permutation(1, 2, 0))
    assert(c == Permutation(1, 2, 0, 3))

    assert(c != Cycle(1, 2, 3))
    assert(c != Identity)
  }

  it must "work for transpositions" in {
    val t = Transposition(0, 1)
    assert(t == t)
    assert(t == Transposition(1, 0))
    assert(t == Transposition(0, 1))
    assert(t == Cycle(0, 1))
    assert(t == Permutation(1, 0))
    assert(t == Permutation(1, 0, 2))

    assert(t != Identity)
    assert(t != Transposition(1, 2))
  }

  it must "work with distinct and toSet" in {

    val c = Cycle(0, 1)
    val cb = Cycle(1, 0)
    val t = Transposition(0, 1)
    val p = Permutation(1, 0)

    val ps = List(c, t, p, c, t, p, cb)

    val set_ps = Set(c, t, p, c, t, p, cb)
    val ps_toSet = ps.toSet
    val set_psstar = Set(ps: _*)
    val ps_distinct = ps.distinct

    assert(c == cb)
    assert(c == t)
    assert(c == p)

    assert(1 == set_ps.size)
    assert(1 == ps_toSet.size)
    assert(1 == set_psstar.size)
    assert(1 == ps_distinct.size)

    assert(set_ps == ps_toSet)
    assert(set_ps == set_psstar)
    assert(set_ps.toList == ps_distinct)
  }

  "hashCode" must "be consistent with equals" in {

    val c = Cycle(0, 1)
    val cb = Cycle(1, 0)
    val t = Transposition(0, 1)
    val p = Permutation(1, 0)

    assert(c == cb)
    assert(c == t)
    assert(c == p)


    assert(c.hashCode == cb.hashCode)
    assert(c.hashCode == t.hashCode)
    assert(c.hashCode == p.hashCode)

  }


  "elements" must "" in {
    assert(pg5.elements == (0 to 4))
  }
  "identity is order 1" must "" in {
    assert(pg5.identity.order == 1)
  }
  "swaps are self inverse" must "" in {
    assert(swap01.inverse == swap01)
  }
  "swaps are order 2" must "" in {
    assert(swap01.order == 2)
  }
  "swap12 is a 2-cycle" must "" in {
    assert(cycle01 == swap01)
  }
  "cycle12 is normalized" must "" in {
    assert(cycle01.normalized == cycle01)
  }
  "different orders imply == cycles" must "" in {
    assert(cycle01 == cycle10)
    assert(cycle01.normalized == cycle10.normalized)
  }
  "rotation is order 5" must "" in {
    val r = pg5.rotation
    val n = r.order
    assert(n == 5)
  }
  "rotation inverse is order 5" must "" in {
    assert(pg5.rotation.inverse.order == 5)
  }
  "disjoint 2- and 3-cycles commute" must "" in {
    val p3 = cycle123
    val p2 = cycle04
    assert(p3.fixedPoints.intersect(p2.fixedPoints).isEmpty)
    assert(p3 * p2 == p2 * p3)
  }
  "identity powers size == 1" must "" in {
    assert(pg5.identity.order == 1)
  }
  "identity = identity.inverse" must "" in {
    assert(pg5.identity == pg5.identity.inverse)
  }

  "cycle (01) on (0 to 5)" must "have expected properties" in {
    val c = Cycle(0, 1)
    val p1 = c.order == 2
    val p2 = c.inverse == c
    val p3 = c.powers.size == 2
    val p4 = c * c == c.identity
    assert(p1)
    assert(p2)
    assert(p3)
    assert(p4)
    assert(c.actingOn(List(1, 2, 3, 4, 5)) == List(2, 1, 3, 4, 5))
  }

  "cycle (012) on (0 to 5)" must "have expected properties" in {
    val c = Cycle(0, 1, 2)
    val p = c.powers.toVector

    assert(c == Permutation(1, 2, 0, 3, 4))
    assert(c * c == Permutation(2, 0, 1, 3, 4))
    assert(c * c * c == c.identity)
    assert(c * c * c == (c ^ 3))
    assert(c * c == (c ^ 2))
    assert(c.actingOn(List(1, 2, 3, 4, 5)) == List(3, 1, 2, 4, 5))

    assert(c.inverse == (c ^ 2))
    assert(c.powers.size == 3, c.powers)
    assert(c.order == 3)
  }
  "actingOn" must "work for identity" in {
    val p = PermGroup(5).identity
    val s = List(1, 2, 3)
    val t = p.actingOn(s)
    assert(s == t)
  }
  it must "work for transposition" in {
    val p = PermGroup(5).transpose(0, 1)
    val s = List(1, 2, 3)
    val t = p.actingOn(s)
    assert(t == List(2, 1, 3))
  }
  it must "work for 3-cycle" in {
    val p = Cycle(0, 1, 2)
    val s = List(1, 2, 3, 4, 5)
    val t = p.actingOn(s)
    assert(t == List(3, 1, 2, 4, 5))
  }
  it must "work for composition" in {
    val p = Cycle(0, 1)
    val q = Cycle(1, 2, 3)
    val s = List(1, 2, 3, 4, 5)
    val ps = p.actingOn(s)
    val qs = q.actingOn(s)
    val pqs = p.actingOn(qs)
    val qps = q.actingOn(ps)
    val pq = p * q
    val qp = q * p
    val pq_s = pq.actingOn(s)
    val qp_s = qp.actingOn(s)
    assert(pqs == pq_s)
    assert(qps == qp_s)

  }

  "basicTranspositions" must "be identity on a basic transposition" in {
    val t = Transposition(2,3)
    assert(t.isBasic)
    assert(Perm.basicTranspositions(t) == List(t))
  }

  it must "work for (2 4)" in {
    val t = Transposition(2, 4)
    assert(!t.isBasic)
    val b = Perm.basicTranspositions(t)
    assert(b.size == 3)
    assert(b == List(Cycle(2,3),Cycle(3,4),Cycle(2,3)))
    val t2 = b.fold(Identity)(_ * _)
    assert(t2 == t)
  }

  it must "work for (1 3)(2 4)" in {
    val t = Transposition(1, 3) * Transposition(2, 4)
    assert(!t.isTransposition)
    val b = Perm.basicTranspositions(t)
    val t2 = b.fold(Identity)(_ * _)
    assert(t2 == t)
    assert(b.size == 6)
    assert(b == List(Cycle(1, 2), Cycle(2, 3), Cycle(1, 2), Cycle(2, 3), Cycle(3, 4), Cycle(2, 3)))
  }
}
