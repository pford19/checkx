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
  val swaps = for (n <- Gen.choose(0, 3); m <- Gen.choose(n+1, 4)) yield pg5.transpose(n, m)
  val threeCycles = for (i <- g04; j <- g04 if i != j; k <- g04 if i != k && j != k) yield Cycle(i, j, k)


  // powers properties
  property("identity powers size == 1") = pg5.identity.order == 1
  property("p and inverse have same order") = forAll(PermutationGenerator.uniformPermutations(6)) { p: Perm => p.order == p.inverse.order }

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
    val pows = (0 to p.order-1).map(p^_).toList
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
    val pows = (0 to p.order-1).map(p^_).toList
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
  property("2-cycles are cycles") = forAll(swaps) (_.isCycle)

  property("(012)(34) is not a cycle") = {
    val s = pg5.transpose(3, 4)
    val t = Cycle(0, 1, 2)
    !(s * t).isCycle && !(t * s).isCycle
  }

  property("(012) has orbits (012)(3)(4)") = {
    val p = Cycle(0, 1, 2)
    p.orbit(0) == Cycle(0, 1, 2) &&
      p.orbit(3) == Cycle(3) &&
      p.orbit(4) == Cycle(4)
  }

  property("(012)(34) has 2 orbits") = {

    val s = pg5.transpose(3, 4)
    val t = Cycle(0, 1, 2)
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
    val p = c
    val x = shrinkCycle(c)
    val y = shrinkOnce(p)
    val z = shrinkTree(p)

    (p.cycles.size == 1
      && p.cycles.head == c
      && x == Cycle(1, 2, 3, 4)
      && y.size == 1
      && y.head == x
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

  implicit val permutationShrinker: Shrink[Perm] = Shrink {
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

  property("(123)*(12)==(13)") = {
    Cycle(1, 2, 3) * Cycle(1, 2) == Cycle(1, 3)
  }

  def pairGenerator(n: Int, m: Int): Gen[(Perm, Perm)] = {
    for {
      p1 <- PermutationGenerator.uniformPermutations(n)
      p2 <- PermutationGenerator.uniformPermutations(m)
    } yield (p1, p2)
  }


  property("different degree permutations can be multiplied") = forAll(pairGenerator(5, 7)) { case (p5, p7) =>
    val x = ('0' to '6') // first 7 digits

    val p57 = p5 * p7
    val p75 = p7 * p5

    val p5_p7_x = p5 actingOn (p7 actingOn x)
    val p7_p5_x = p7 actingOn (p5 actingOn x)
    val p57_x = p57 actingOn x
    val p75_x = p75 actingOn x

    val assertion = (p57).degree == 7 &&
      (p75).degree == 7 &&
      p5_p7_x == p57_x &&
      p7_p5_x == p75_x
    if (!assertion) {
      println(s"p5 = $p5")
      println(s"p7 = $p7")
      println(s"p57 = $p57")
      println(s"p5_p7_x = $p5_p7_x")
      println(s"p57_x   = $p57_x")
      println(s"p75 = $p75")
      println(s"p7_p5_x = $p7_p5_x")
      println(s"p75_x   = $p75_x")
    }
      assertion

  }

  property("actOn works on a specific case") = {
    val p1 = (Cycle(1, 2, 3) actingOn List(0, 1, 2, 3)) == List(0, 3, 1, 2)
    val p2 = (Permutation(0, 2, 3, 1) actingOn List(0, 1, 2, 3)) == List(0, 3, 1, 2)
    p1 && p2
  }

  property("actOn works in general") = forAll(PermutationGenerator.uniformPermutations(5)) { p =>
    val s = p actingOn (0 to 4).toList
    val assertion = (0 to 4).forall( i=> i == p.pmap(s(i)))
    assertion
  }

  property("actOn works with inverse") = forAll(PermutationGenerator.uniformPermutations(5)) { p =>
    val s = p.actingOn(0 to 4)
    val pi = p.inverse
    val t = pi.actingOn(s)
    val assertion = t == (0 to 4)
    assertion
  }
}
