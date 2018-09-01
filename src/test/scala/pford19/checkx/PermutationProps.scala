package pford19.checkx

import org.scalacheck._
import org.scalacheck.Prop.{AnyOperators, BooleanOperators, collect, exists, forAll}
import org.scalacheck.util.ConsoleReporter

import scala.util.{Failure, Success, Try}

object PermutationProps extends Properties("permutations") {


  override def overrideParameters(p: Test.Parameters): Test.Parameters = Test.Parameters.default
    .withTestCallback(ConsoleReporter(verbosity = 2, columnWidth = 0))
    .withMinSuccessfulTests(100)

  val pg5 = PermGroup(5)
  val swap01 = Permutation(1, 0, 2, 3, 4)
  val cycle01 = Cycle(0, 1)
  val cycle10 = Cycle(1, 0)
  val cycle5 = Cycle(0, 1, 2, 3, 4)
  val reverse5 = Permutation(4, 3, 2, 1, 0)
  val cycle04 = Cycle(0, 4)
  val cycle13 = Cycle(1, 3)
  val cycle123 = Cycle(1, 2, 3)


  // Generators
  val g04 = Gen.choose(0, 4)
  val swaps = for (n <- Gen.choose(0, 4); m <- Gen.choose(0, 4) if m != n) yield pg5.transpose(n, m)
  val threeCycles = for (i <- g04; j <- g04 if i != j; k <- g04 if i != k && j != k) yield Cycle(i, j, k).asPermutation(5)


  // Static props
  property("static: elements") = pg5.elements == (0 to 4)
  property("static: identity is order 1") = pg5.identity.order == 1
  property("static: identity has n fixed points") = pg5.identity.fixedPoints.size == pg5.degree
  property("static: swaps are self inverse") = swap01.inverse == swap01
  property("static: swaps are order 2") = swap01.order == 2
  property("static: swap12 is a 2-cycle") = cycle01.asPermutation(swap01.degree) == swap01
  property("static: cycle12 is normalized") = cycle01.normalized == cycle01
  property("static: different orders imply != cycles") = cycle01 != cycle10 && cycle01.normalized == cycle10.normalized
  property("static: full cycle is order 5") = pg5.rotation.order == 5
  property("static: full cycle inverse is order 5") = pg5.rotation.inverse.order == 5
  property("static: full cycle inverse is order 5") = pg5.rotation.inverse.order == 5
  property("static: disjoint 2- and 3-cycles commute") = {
    val p3 = cycle123.asPermutation(5)
    val p2 = cycle04.asPermutation(5)
    p3.fixedPoints.intersect(p2.fixedPoints).isEmpty &&
      p3 * p2 == p2 * p3
  }

  // powers properties
  property("identity powers size == 1") = pg5.identity.powers.size == 1
  property("p and inverse have same order") = forAll(PermutationGenerator.uniformPermutations(6)) { p: Permutation => p.powers.size == p.inverse.powers.size }
  property("powers(0) == identity") = forAll(PermutationGenerator.uniformPermutations(6)) { p: Permutation => p.powers.head == p.identity }
  property("powers,last == inverse") = forAll(PermutationGenerator.uniformPermutations(6)) { p: Permutation => p.powers.last == p.inverse }
  property("powers.size == order") = forAll(PermutationGenerator.uniformPermutations(6)) { p: Permutation => p.powers.size == p.order }

  // inverse properties
  property("identity = identity.inverse") = pg5.identity == pg5.identity.inverse
  property("p = p.inverse.inverse") = forAll(PermutationGenerator.uniformPermutations(6)) { p => p.inverse.inverse == p }
  property("p * p.inverse == identity") = forAll(PermutationGenerator.uniformPermutations(6)) { p => p * p.inverse == p.identity && p.inverse * p == p.identity }

  //


  // properties of reverse on permgroups of size 2 to 7
  for (n <- (2 to 7)) {
    property(s"reverse of permgroup($n) is order 2 with ${n % 2} fixed points") = {
      val r = PermGroup(n).reverse
      r.order == 2 && r.fixedPoints.size == n % 2
    }
  }

  // permutation of empty set
  property("permutations of degree 0 (acting on empty set)") = {
    val pg0 = PermGroup(0)
    val id = pg0.identity
    val p = Permutation(List())
    val rots = pg0.rotations
    val revrots = pg0.reverseRotations
    val swaps = pg0.transpositions
    val pows = p.powers
    p.isIdentity &&
      p == id &&
      p.degree == 0 &&
      rots.size == 1 &&
      revrots.size == 1 &&
      swaps.size == 0 &&
      pows.size == 1
  }
  property("permutations of degree 1 (acting on singleton set)") = {
    val pg1 = PermGroup(1)
    val id = pg1.identity
    val p = Permutation(List(0))
    val rots = pg1.rotations.toList
    val revrots = pg1.reverseRotations.toList
    val swaps = pg1.transpositions.toList
    val pows = p.powers.toList
    p.isIdentity &&
      p == id &&
      p.degree == 1 &&
      rots.size == 1 &&
      revrots.size == 1 &&
      swaps.size == 0 &&
      pows.size == 1 &&
      true
  }

  // Generated properties

  property("2-cycles are self inverse") = forAll(swaps) { swap => {

    swap.inverse == swap
    swap.order == 2
    swap != swap.identity
  }
  }

  property("3-cycles in PermGroup(5) do not commute") = forAll(
    for (c1 <- threeCycles; c2 <- threeCycles) yield (c1, c2)
  ) { case (c1, c2) => c1.powers.contains(c2) == (c1 * c2 == c2 * c1)
  }

  property("3-cycles are cycles") = forAll(threeCycles) {
    _.isCycle
  }
  property("2-cycles are cycles") = forAll(swaps) {
    _.isCycle
  }

  property("(012)(34) is not a cycle") = {
    val s = pg5.transpose(3, 4)
    val t = Cycle(0, 1, 2).asPermutation(5)
    !(s * t).isCycle && !(t * s).isCycle
  }

  property("(012) has orbits (012)(3)(4)") = {
    val p = Cycle(0, 1, 2).asPermutation(5)
    p.orbit(0) == Cycle(0, 1, 2) &&
      p.orbit(3) == Cycle(3) &&
      p.orbit(4) == Cycle(4)
  }

  property("(012)(34) has 2 orbits") = {

    val s = pg5.transpose(3, 4)
    val t = Cycle(0, 1, 2).asPermutation(5)
    val st = s * t
    val ts = t * s

    st == ts &&
      st.order == 6 &&
      st.orbit(0) == st.orbit(1) &&
      st.orbit(0) == st.orbit(2) &&
      st.orbit(3) == st.orbit(4) &&
      st.orbit(0) != st.orbit(3)
  }

  // weightedPermutations generator properties.

  property("weightedPermutations include identity") =
    exists(PermutationGenerator.weightedPermutations(10)) { p => p == p.identity }

  property("weightedPermutations include reverse") =
    exists(PermutationGenerator.weightedPermutations(10)) { p => p == PermGroup(10).reverse }

  property("weightedPermutations include a swap") =
    exists(PermutationGenerator.weightedPermutations(10)) { p =>
      p.nonFixedPoints.size == 2
    }

  property("weightedPermutations include a permutation with no fixed points") =
    exists(PermutationGenerator.weightedPermutations(10)) { p =>
      p.nonFixedPoints.size == 0
    }

  // permutation properties
  property("identity iff all fixed points") = forAll(PermutationGenerator.uniformPermutations(10)) { p =>
    (p.fixedPoints.size == p.degree) == (p == p.identity)
  }
  property("fixed and nonfixed partition") = forAll(PermutationGenerator.uniformPermutations(10)) { p =>
    val fixed = p.fixedPoints
    val nonfixed = p.nonFixedPoints

    ((fixed.size + nonfixed.size) == p.degree) && fixed.intersect(nonfixed).isEmpty

  }

  // Shrink properties
  property("shrinkCycle shrinks cycle size by 1 or 2") = forAll(PermutationGenerator.weightedPermutations(10)) { p =>
    val pairs = for {cycle <- p.cycles} yield (cycle, PermutationGenerator.shrinkCycle(cycle))
    pairs.forall { case (c, s) => c.size match {
      case 0 => 0 == s.size
      case 1 => 0 == s.size
      case 2 => 0 == s.size
      case n => n - 1 == s.size
    }
    }
  }

  property("shrinkOnce shrinks") = forAll(PermutationGenerator.uniformPermutations(10)) { p =>
    val psize = p.nonFixedPoints.size
    val shrinks = PermutationGenerator.shrinkOnce(p)
    (!p.isIdentity) ==> {

      // shrink reduces nonfixed points by 1 or 2. 2 in the case that a 2-cycle is replaced by a 1-cycle, 1 otherwise n+1-cycle replaced by an n-cycle for n>1.
      shrinks.forall { q =>
        val qsize = q.nonFixedPoints.size
        qsize < psize

      }
    }
  }

  property("shrink cycle (01234)") = {
    import PermutationGenerator._
    val c = Cycle(0, 1, 2, 3, 4)
    val p = c.asPermutation(5)
    val x = shrinkCycle(c)
    val y = shrinkOnce(p)
    val z = shrinkTree(p)

    (p.cycles.size == 1
      && p.cycles.head == c
      && x == Cycle(1, 2, 3, 4)
      && y.size == 1
      && y.head == x.asPermutation(5)
      && z.size == 4
      )
  }

  property("big shrink") = forAll(PermutationGenerator.uniformPermutations(10)) { p =>

    (!p.isIdentity) ==> {

      val shrinks = PermutationGenerator.shrinkTree(p)

      ("cycle" |: ((p.isCycle && p.cycles.head.size > 2) ==> (shrinks.size == p.cycles.head.size - 1))) && // shrink count of a cycle is easy
        ("set" |: (shrinks.toSet.size == shrinks.size)) &&
        ("fixed" |: shrinks.forall(_.nonFixedPoints.size < p.nonFixedPoints.size)) &&
        ("identity contained" |: shrinks.contains(p.identity)) &&
        true
    }
  }

  property("shrink identity(10)") = {
    import PermutationGenerator._

    val id = Permutation.identity(10)
    val cycles = id.cycles
    val cycle = cycles.head
    val x = shrinkCycle(cycle)
    val y = shrinkOnce(id)
    val z = shrinkTree(id)

    val p1: Prop = cycles.size == 1
    val p2 = cycle.size == 0
    val p3 = x.size == 0
    val p4 = y.size == 0
    val p5 = z.size == 0

    p1 :| "cycles.size == 1" &&
      p2 :| "cycle is 0-cycle" &&
      p3 :| "shrinkCycle is 0-cycle" &&
      p4 :| "shrinkOnce is empty" &&
      p5 :| "shrinkTree is empty" &&
      true
  }

  // test shrinking of an intentionally failing false property

  implicit val permutationShrinker: Shrink[Permutation] = Shrink {
    x => PermutationGenerator.shrinkTree(x).toStream
  }

  property("at most N/2 cycles for even N") = forAll(PermutationGenerator.uniformPermutations(8)) { p =>
    Prop.collect(p.cycles.size) {
      p.cycles.size <= 8 / 2
    }

  }
  property("as many as N/2 cycles for even N") = exists(PermutationGenerator.uniformPermutations(6)) { p =>
    Prop.collect(p.cycles.size) {
      p.cycles.size == 6 / 2
    }
  }

  property("at most (N-1)/2 cycles for odd N") = forAll(PermutationGenerator.uniformPermutations(11)) { p =>
    Prop.collect(p.cycles.size) {
      p.cycles.size <= 10 / 2
    }

  }
  property("as many as (N-1)/2 cycles for odd N") = exists(PermutationGenerator.uniformPermutations(7)) { p =>
    Prop.collect(p.cycles.size) {
      p.cycles.size == 7 / 2
    }
  }

  property("permGenerator works") = {
    def testn(n: Int): Boolean = {
      val nfact = Combinatorics.factorial(n).toInt
      val x = PermGenerator.permGenerator(n)
      val plist = (1 to Combinatorics.factorial(n).toInt).map(_ => x())
      plist.toSet.size == nfact
    }

    testn(2) &&
      testn(3) &&
//      testn(4) &&
//      testn(5) &&
    true
  }

  property("permGenerator is unbounded") = {
    def testnm(n: Int, m: Int): Boolean = {
      val pg = PermGenerator.permGenerator(n)
      val perms = for (_ <- (1 to m)) yield pg()
      perms.size == m
    }

    testnm(4, 400) // there are 24 permutationson 4 elements, 400 just keeps cycling
  }

  property("perfStream generates n! permutations and starts with identity") = {
    def testn(n: Int): Boolean = {
      val nfact = Combinatorics.factorial(n).toInt
      val x = PermGenerator.permStream(n)
      val plist = x.take(nfact)
      plist.head.isIdentity &&
        plist.toSet.size == nfact

    }

    testn(2) && testn(3) && testn(4) && testn(5)
  }
  property("perfStream is repeating cycle") = {
    def testn(n: Int): Boolean = {
      val nfact = Combinatorics.factorial(n).toInt
      val x = PermGenerator.permStream(n)
      val plist = x.take(2 * nfact + 1) // should start and end with identity
      println(s"Debug: n=$n, n!=$nfact, ${plist.size}, ${plist.toSet.size}, ${plist.filter(_.isIdentity).size}")

      plist.filter(_.isIdentity).size == 3 &&
        plist.head.isIdentity &&
        plist.last.isIdentity

    }

    testn(2) &&
      testn(3) &&
//      testn(4) &&
//      testn(5) &&
      true
  }

  property("perfIterator head is identity") = {
    val it = PermGenerator.permIterator(5)
    it.hasNext &&
      it.next.isIdentity
  }

  property("perfIterator(n) n in (2 to 5) works") = {
    def testn(n: Int) = {

      val dfact = Combinatorics.factorial(n)
      require(Math.ulp(dfact) < 1.0d, s"n! for n=$n is too large to represent as an exact Double integer: $dfact")
      val nfact = dfact.toLong
      require(nfact <= Int.MaxValue, s"n! for n=$n is too larget to represent as an Int: ${nfact}")

      val it = PermGenerator.permIterator(n)
      val perms = it.take(nfact.toInt).toList // 120 = 5!
      println(s"DEBUG perms.size == ${perms.size}")
      println(s"DEBUG perms.set.size == ${perms.toSet.size}")
      val iterator_exhausted = !it.hasNext
      val expected_size = perms.size == nfact
      val expected_head = perms.head.isIdentity
      val expected_set_size = perms.toSet.size == nfact

      iterator_exhausted &&
        expected_size &&
        expected_set_size &&
        expected_head &&
        true
    }

    //testn(5) &&
    //testn(4) &&
    testn(3) &&
      testn(2) &&
      true
  }

  property("permGenerator(-1, 0, 1) all work expected") = {
    val neg1fails = Try {
      PermGenerator.permGenerator(-1)
    } match {
      case Success(pg) => false
      case Failure(e) => true
    }
    val zerosuccess = Try {
      PermGenerator.permGenerator(0)
    } match {
      case Success(pg) => true
      case Failure(e) => false
    }
    val onesuccess = Try {
      PermGenerator.permGenerator(1)
    } match {
      case Success(pg) => true
      case Failure(e) => false
    }
    neg1fails && zerosuccess && onesuccess
  }

  //  property("permGenerator(500) performs some basic operations") = {
  //    val it = PermGenerator.permIterator(100)
  //    val totake = 25
  //    val t500 = it.take(totake).toList
  //    t500.head.isIdentity &&
  //    t500.size == totake &&
  //    t500.toSet.size == totake &&
  //    t500.head != t500.last &&
  //    true
  //
  //  }

  //  property("permGenerator for degree 6") = {
  //    val f = PermGenerator.permGenerator(6)
  //    (0 to 722).foreach(_ => f.apply())
  //    true
  //  }
  //
  //  property("permGenerator for degree 7") = {
  //    val f = PermGenerator.permGenerator(7)
  //    (0 to 5040+2).foreach(_ => f.apply())
  //    true
  //  }


}
