package pford19.checkx

import org.scalacheck._
import org.scalacheck.util.ConsoleReporter
import org.scalatest.FlatSpec
import org.scalatest.MustMatchers


import scala.util.{Failure, Success, Try}


class PermGeneratorProps extends FlatSpec {


  //  override def overrideParameters(p: Test.Parameters): Test.Parameters = Test.Parameters.default
  //    .withTestCallback(ConsoleReporter(verbosity = 2, columnWidth = 0))
  //    .withMinSuccessfulTests(100)

  "permGenerator" must "work" in {

    def testn(n: Int) = {
      val nfact = Combinatorics.factorial(n).toInt
      val x = PermGenerator.permGenerator(n)
      val plist = (1 to nfact - 1).map(_ => x())
      assert(nfact - 1 == plist.size, "plist size")
      assert(nfact - 1 == plist.toSet.size, "plist set size")
      val nextP = x()
      assert(nextP.isIdentity, "last perm in cycle is identity")
      assert(!plist.contains(nextP), "identity not in plist")
    }

    (2 to 5).foreach(testn(_))
  }

  it must "be unbounded" in {
    def testnm(n: Int, m: Int) = {
      val pg = PermGenerator.permGenerator(n)
      val perms = for (_ <- (1 to m)) yield pg()
      assert(perms.size == m)
    }

    testnm(4, 24 * 3 + 1) // there are 24 permutationson 4 elements, cycle 3 times plus 1
  }

  it must "all work expected for degree -1" in {
    val neg1fails = Try {
      PermGenerator.permGenerator(-1)
    } match {
      case Success(pg) => false
      case Failure(e) => true
    }
    assert(neg1fails)
  }


  it must "work for degree 6" in {
    val f = PermGenerator.permGenerator(6)
    (0 to 722).foreach(_ => f.apply())
    true
  }

  it must "for degree 7" in {

    val f = PermGenerator.permGenerator(7)
    (0 to 5040 + 2).foreach(_ => f.apply())
    true
  }

  "permStream" must "generates n! permutations and starts with identity" in {
    def testn(n: Int) = {
      val nfact = Combinatorics.factorial(n).toInt
      val x = PermGenerator.permStream(n)
      val plist = x.take(nfact)
      assert(plist.head.isIdentity)
      assert(plist.toSet.size == nfact)

    }

    testn(2);
    testn(3);
    testn(4);
    testn(5)
  }

  it must "be unbounded cycle" in {
    def testn(n: Int) = {
      val nfact = Combinatorics.factorial(n).toInt
      val x = PermGenerator.permStream(n)
      val plist = x.take(2 * nfact + 1) // should start and end with identity
      //println(s"Debug: n=$n, n!=$nfact, ${plist.size}, ${plist.toSet.size}, ${plist.filter(_.isIdentity).size}")

      assert(plist.filter(_.isIdentity).size == 3)
      assert(plist.head.isIdentity)
      assert(plist.last.isIdentity)

    }

    testn(2)
    testn(3)
    //      testn(4)
    //      testn(5)

  }
  //
  "permIterator" must " head is identity" in {
    val it = PermGenerator.permIterator(5)
    assert(it.hasNext)
    assert(it.next.isIdentity)
  }

  it must "performs some basic operations for degree 100" in {
    val it = PermGenerator.permIterator(100)
    val totake = 500

    val t500 = it.take(totake).toList
    assert(t500.head.isIdentity)
    assert(t500.size == totake)
    assert(t500.toSet.size == totake)
    assert(t500.head != t500.last)
  }

  it must "work for degree (2 to 7)" in {
    def testn(n: Int) = {

      val dfact = Combinatorics.factorial(n)
      require(Math.ulp(dfact) < 1.0d, s"n! for n=$n is too large to represent as an exact Double integer: $dfact")
      val nfact = dfact.toLong
      require(nfact <= Int.MaxValue, s"n! for n=$n is too larget to represent as an Int: ${nfact}")

      val it = PermGenerator.permIterator(n)
      val perms = it.take(nfact.toInt).toList // 120 = 5!
      //      println(s"DEBUG perms.size == ${perms.size}")
      //      println(s"DEBUG perms.set.size == ${perms.toSet.size}")
      assert(!it.hasNext)
      assert(perms.size == nfact)
      assert(perms.head.isIdentity)
      assert(perms.toSet.size == nfact)
    }

    (2 to 7).foreach(testn(_))
    true
  }


  "degree 0 and 1" must "be trival" in {
    def testTrivial(degree: Int) = {
      val g = PermGenerator.permGenerator(degree)
      val s = PermGenerator.permStream(degree)
      val t = PermGenerator.permIterator(degree)
      // generator is identity every time
      assert(g.apply().isIdentity)
      assert(g.apply().isIdentity)
      // stream is identity every time
      assert(s.head.isIdentity)
      assert(s.tail.head.isIdentity)
      // iterator is size 1, first element is identity
      assert(t.hasNext)
      assert(t.next.isIdentity)
      assert(!t.hasNext)
    }

    testTrivial(0)
    testTrivial(1)
  }

  "unBoundedtateIterator" must "in unbounded and cycles" in {

    def test(degree: Int): Unit = {
      assert(degree >= 2)
      val factorial = Combinatorics.longFactorial(degree).get
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermGenerator.unboundedStateIterator(degree)
      (1L to 3 * factorial + 1).foreach { i =>
        assert(it.hasNext)
        val s = it.next
        if(i == 1) assert(!s.perm.isIdentity)
        val p = s.perm
        val j = s.callCount
        assert((j % factorial != 0) || p.isIdentity)
      }
    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure
    test(2)
    test(3)
    test(4)
    test(5)
  }
  it must "have identity at end of cycle" in {

    def test(degree: Int): Unit = {
      assert(degree >= 2)
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermGenerator.unboundedStateIterator(degree)
      it.drop(factorial-1)
      assert(it.next.perm.isIdentity)
    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure
    test(2)
    test(3)
    test(4)
    test(5)
  }

  it must "be undbounded" in {

    def test(degree: Int): Unit = {
      assert(degree >= 2)
      val factorial = Combinatorics.longFactorial(degree).get
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermGenerator.unboundedStateIterator(degree)
      (1L to 3 * factorial + 1).foreach { i =>
        assert(it.hasNext)
      }
    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure
    test(2)
    test(3)
    test(4)
    test(5)
  }

  "unBoundedtateIterator" must "cycle" in {

    def test(degree: Int): Unit = {
      assert(degree >= 2)
      val factorial = Combinatorics.longFactorial(degree).get.toInt
      assert(factorial > 0, s"test requires small degree, $degree is way too big")
      assert(factorial < 100000, s"test requires small degree, $degree is too big")
      val it = PermGenerator.unboundedStateIterator(degree)
      val p1 = it.take(factorial)
      it.drop(factorial)
      val p2 = it.take(factorial)
      p1 == p2
    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure
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
      val it = PermGenerator.stateIterator(degree)
      assert(it.hasNext)
      assert(!it.next.perm.isIdentity)
    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure
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
      val it = PermGenerator.stateIterator(degree)
      val states = it.toList
      assert(states.size == factorial)
      assert(!states.head.perm.isIdentity)
      assert(states.last.perm.isIdentity)
      assert(states.map(_.perm).toSet.size == states.size)
      assert(states.map(_.callCount).toSet.size == states.size)

    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure

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
      val it = PermGenerator.permutationIterator(degree)
      val perms = it.toList
      assert(perms.size == factorial)
      assert(!perms.head.isIdentity)
      assert(perms.last.isIdentity)
      assert(perms.toSet.size == perms.size)
      assert(perms.toSet.size == perms.size)

    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure

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
      val it = PermGenerator.transpositionIterator(degree)
      val perms = it.toList
      assert(perms.size == factorial-1)
      assert(perms.map(_.size).toSet == Set(2))

    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure

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
      val pit = PermGenerator.permutationIterator(degree)
      val tit = PermGenerator.transpositionIterator(degree)

      val perms = pit.toList
      val trans = tit.toList

      val pt = perms.zip(trans).map { case (p, t) => p * t}
      assert(pt == perms.tail)

    }

    // case 0 and 1 are trival (singleton) permutation groups
    // by design, the state mechanism doesn't handle these
    // the permutation and transposition iterators do.
    Try { test(0)}.isFailure
    Try { test(1)}.isFailure

    test(2)
    test(3)
    test(4)
    test(5)
  }

}
