package pford19.checkx

import org.scalacheck._
import org.scalacheck.util.ConsoleReporter
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers


import scala.util.{Failure, Success, Try}

/** Test PermIterator: states, unbounded states, permutations, transitions, IterationState
  *
  */

class PermIteratorTest extends FlatSpec {

  "unBoundedtateIterator" must "is unbounded and cycles" in {

    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.unboundedStateIterator(degree)
      (1L to 3 * factorial + 1).foreach { i =>
        assert(it.hasNext)
        val s = it.next
        if (i == 1 && degree >= 2) assert(!s.perm.isIdentity)
        val p = s.perm
        val j = s.callCount
        assert((j % factorial != 0) || p.isIdentity)
      }
    }

    // case 0 and 1 are trival (singleton) permutation groups
    test(0)
    test(1)
    test(2)
    test(3)
    test(4)
    test(5)
  }

  it must "have identity at end of cycle" in {

    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.unboundedStateIterator(degree)
      it.drop(factorial - 1)
      assert(it.next.perm.isIdentity)
    }

    // case 0 and 1 are trival (singleton) permutation groups
    test(0)
    test(1)
    test(2)
    test(3)
    test(4)
    test(5)
  }

  it must "be unbounded" in {

    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.unboundedStateIterator(degree)
      (1L to 3 * factorial + 1).foreach { i =>
        assert(it.hasNext)
      }
    }

    // case 0 and 1 are trival (singleton) permutation groups
    test(0)
    test(1)
    test(2)
    test(3)
    test(4)
    test(5)
  }

  it must "handle degree 0" in {
    val it = PermIterator.unboundedStateIterator(0)
    assert(it.hasNext)
    assert(it.next.perm.isIdentity) // first permuation is identity
    assert(it.next.perm.isIdentity) // second permutation is identity, etc.
  }

  it must "handle degree 1" in {
    val it = PermIterator.unboundedStateIterator(1)
    assert(it.hasNext)
    assert(it.next.perm.isIdentity) // first is identity
    assert(it.next.perm.isIdentity) // second is identity, etc.
  }

  it must "cycle" in {

    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.unboundedStateIterator(degree)
      val p1 = it.take(factorial).map(_.perm).toList
      it.drop(factorial)
      val p2 = it.take(factorial).map(_.perm).toList
      assert(p1 == p2)
    }

    // case 0 and 1 are trival (singleton) permutation groups
    test(0)
    test(1)
    test(2)
    test(3)
    test(4)
    test(5)
  }

  "stateIterator (bounded)" must "not start with identity" in {
    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.stateIterator(degree)
      assert(it.hasNext)
      assert(!it.next.perm.isIdentity)
    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try {
      test(0)
    }.isFailure
    Try {
      test(1)
    }.isFailure
    test(2)
    test(3)
    test(4)
    test(5)
  }

  it must "have size degree! and last perm is identity" in {
    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.stateIterator(degree)
      val states = it.toList
      assert(states.size == factorial)
      if (degree >= 2) assert(!states.head.perm.isIdentity)
      assert(states.last.perm.isIdentity)
      assert(states.map(_.perm).toSet.size == states.size)
      assert(states.map(_.callCount).toSet.size == states.size)

    }

    // case 0 and 1 are trival (singleton) permutation groups
    test(0)
    test(1)
    test(2)
    test(3)
    test(4)
    test(5)
  }

  "permutationGenerator" must "have size degree! and last is identity" in {
    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.permutationIterator(degree)
      val perms = it.toList
      assert(perms.size == factorial)
      if (degree >= 2) assert(!perms.head.isIdentity)
      assert(perms.last.isIdentity)
      assert(perms.toSet.size == perms.size)
      assert(perms.toSet.size == perms.size)

    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    test(0)
    test(1)

    test(2)
    test(3)
    test(4)
    test(5)
  }

  "transpositionGenerator" must "have size degree!-1 and be all 2-cycles" in {
    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermIterator.transpositionIterator(degree)
      val trans = it.toList
      assert(trans.size == factorial - 1)

    }

    // case 0 and 1 are trival (singleton) permutation groups
    test(0)
    test(1)

    test(2)
    test(3)
    test(4)
    test(5)
  }

  it must "be consistent with permutation generator" in {
    def test(degree: Int): Unit = {
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val pit = PermIterator.permutationIterator(degree)
      val tit = PermIterator.transpositionIterator(degree)

      val perms = pit.toList
      val trans = tit.toList

      val pt = perms.zip(trans).map { case (p, t) => t* p }
      assert(pt == perms.tail)

    }

    // case 0 and 1 are trival (singleton) permutation groups
    test(0)
    test(1)

    test(2)
    test(3)
    test(4)
    test(5)
  }

  it must "be interesting" in {
    def xxx(n: Int): Unit = {
      println(s"=======> stateIterator($n)")
      val it = PermIterator.stateIterator(n)
      while (it.hasNext) {
        val s = it.next
        val t = s.lastTransposition
        val p = s.perm
        val j = s.callCount
        val shuffle = p.actingOn(0 to n-1)
        println(f"[$n] $j%3d: ${t.j} ${t.k}  $shuffle ${p.cyclicRepresentation}")
      }
      println(s"=======> stateIterator($n)")
    }

//    xxx(3)
//    xxx(4)
//    xxx(5)
    xxx(6)
  }


  "permIterator" must "work form some larger values of n" in {
    def test(degree: Int, n: Int) = {
      val it = PermIterator.permutationIterator(degree)
      val expect = Math.min(n, Combinatorics.factorial(degree)).toInt
      val perms = it.take(n).toList
      assert(perms.size == expect)
    }

    test(10, 10)
    test(100, 100)
    test(1000, 1000) // 1000! = Double.Infinity
    //    test(10000, 10000) // SLOW
  }

}
