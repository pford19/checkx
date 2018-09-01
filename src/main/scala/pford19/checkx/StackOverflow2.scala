package pford19.checkx

import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}
import org.scalacheck.Prop.{BooleanOperators, collect, forAll}
import org.scalacheck.util.Pretty

import scala.annotation.tailrec
import org.scalacheck.Prop

object StackOverflowExample2 extends Properties("stackoverflow example") {

  property("shrinks to empty list") = forAll(Gen.const(List(1, 2, 3, 4))) { list => list.size < 0 }

  {


    implicit val nonEmptyIntListShrinker = Shrink {
      ints: List[Int] =>
        Shrink.shrink(ints)
          .filter(_.nonEmpty)
    }

    // `forAll[X]` takes several implicits, including an implicit Shrink[X], which we have just provided
    property("shrink stops with a non-empty list with implicit Shrink") = forAll(Gen.const(List(1, 2, 3, 4))) { list => list.size < 0 }
  }

  def generatorInvariant(list: Iterable[Int]) = list.size > 0

  property("shrink stops with a non-empty list with guard") = forAll(Gen.const(List(1, 2, 3, 4))) { list => generatorInvariant(list) ==> (list.size < 0) }


  def customForAll[T1, P](g1: Gen[T1],
                          shrinkFilter: T1 => Boolean)(f: T1 => P)(implicit p: P => Prop,
                                                                             //s1: Shrink[T1],
                                                                             pp1: T1 => Pretty
                                   ): Prop = {
    {

      implicit val xxx = Shrink { t: T1 =>
        Shrink.shrink(t).filter(shrinkFilter)
      }

      forAll(g1)(f)
    }

  }


  property("shrink stops with a non-empty list with custom forAll") = customForAll(Gen.const(List(1, 2, 3, 4)), generatorInvariant) { list => list.size < 0 }

  // Some notes on Shrink.

  // Practical
  //
  // The Shrink object provide shrinkers for standard types and collections.
  // Shrinkers are wired into forAll as implicits.
  // The Shrink object has a factory apply method for building a new shrinker from a function of type T => Stream[T].
  // Shrinkers know about types, but not about generators.
  // A standard Shrinker has a method shrink with the same signature T => Stream[T].
  // You can unwrap a standard Shrink, map and filter, and rewrap as a new Shrink:
  //    Shrink { t: T => Shrink.shrink(t).map( ... ).filter (...) }
  // or
  //    Shrink { t: T => for ( v <- Shrink.shrink(t); if p(v); u = g(v) ) yield u }
  // or
  //    Shrink { t: T => standard-shrink-for-T .map .filter ... }
  //
  // or
  //    def p: T => Boolean = ... // predicate to filter out some shrunk values
  //    def toU: T => U = ...    // conversion to type U with a standard shrinker
  //    def fromU: U => T = ...  // conversion from type U back to T
  //    def f: T => T = ...      // mapping from T to T
  //    Shrink { t: T => Shrink.shrink(toU(t)).map(fromU).map(f).filter(p) }

  // Examples: Shrinker that yields non-negative numeric lists
  //    Shrink { t: Int => Shrink.shrink(t).map(Math.abs) }
  // Shrink that
  //    Shrink { t: Int => Shrink.shrink(t).map(Math.abs) }

  // Theory

  // A model for Shrinking is as follows.

  // - Assume for type T, function shrink(t: T): Stream[T].
  // - Assume a metric function m(t: T): Int
  // - With the following properties
  //
  //  - property("metric is non-negative") = forAll { t: T => m(t) >= 0 }
  //  - property("shrink reduces the metric") = forAll { t: T => shrink(t).forall(m(_) < m(t) }
  //
  // For a given starting value t0: T, shrink defines a tree. The root of the tree is t0, generation 0, g(0): Set[T]
  // It's children in the tree are the elements shrink(t0), this is generation-1, g(1): Set[T].  Each element of t1 in g1
  // has a child set defined by shrink(t1).
  // Conceptually
  //
  // g(0) = Set(t0)
  // g(1) = g(0).flatMap(shrink)
  // g(2) = g(1).flatMap(shrink)
  // g(n) = g(n-1).flatMap(shrink)

  // Some values are unshrinkable. Any value with a zero m-value is unshrinkable. Likewise any value t for which shrink(t).isEmpty is also
  // considered unshrinkable. It is possible for a value t with m(t) > 0 to nonetheless have an empty shrink stream.
  //
  // The next critical property of the shrink tree is that it is finite.
  // -- property("shrink tree is finite" = forAll { t: T => shrinktree(t).hasDefiniteSize }
  //
  // This is equivalent to that every path through the shrink tree from the the root terminates a leaf node consisting of
  // an unshrinkable value.
  //
  // Some examples: shrink(List(1,2,3,4) includes both lists of reduced size such as (1,2), (3,4), (1, 3, 4), (2, 3, 4), (1, 2, 3), (1, 2, 4), and
  // lists with smaller magnitude values (0, 2, 3, 4), (1, 2, 3, -2).
  //
  // The next critical property of a shrink is that the shrink generations eventually become empty.
  //
  // As Rickard Nilsson puts it in his "ScalaCheck" book, "the shrink method must converge towards an empty stream".
  // (Nilsson, Rickard. ScalaCheck: The Definitive Guide. Artima Press.
  //
  // If T is a compound type, e.g., List[Int] or Map[String,String] then shrinking can take place in multiple "dimensions". For example
  // shrinking a List[Int] shrink either the size of the list or the magnitude of the Int, or both.
  //
  // Shrinking strategies tend to be geometric rather than linear: Dividing by 2 is a faster way to shrink an integer
  // than subtracting 2.
  //
  // The default ScalaCheck
  // shrinkers reduce the magnitude (absolute value) of Integral and Fractional numbers by a factor of 2, and also include
  // sign alternations.
  // Collection sizes are reduced by a factor of 2 as well as reducing the values in the collection while keeping the size constant.


  // The challenge with implementing your own custom Shrinks
  // -- lean source documentation
  // -- not a lot of examples
  // -- The Scalacheck book presents a nice, and complete, custom Shrink example in section 6.2 for a recursive data type.


  //  ! stackoverflow example.shrinks to empty list: Falsified after 0 passed tests.
  //  > ARG_0: List()
  //  > ARG_0_ORIGINAL: List("1", "2", "3", "4")

  //  ! stackoverflow example.shrink stops with a non-empty list: Falsified after 0 passed tests.
  //  > ARG_0: List()
  //  > ARG_0_ORIGINAL: List("1", "2", "3", "4")
  //  Found 2 failing properties.

  if (false) {
    Shrink.shrink(List(1, 2, 3, 4, 5, 6, 7, 8, 9)).foreach(println)
    Shrink.shrink(List(1, 2, 3, 4, 5, 6, 7, 8, 9)).foreach(println)
    Shrink.shrink(List(1, 2)).foreach(println)
    Shrink.shrink(List(19)).foreach(println)
    Shrink.shrink(99).foreach(println)
    Shrink.shrink(99.0).foreach(println)
    Shrink.shrink(('a' to 'd').mkString("")).foreach(println)
  }

  /** Recursive traversal of the entire Shrink tree for `t`.
    * This can get very big. For example,
    *
    * @param f
    * @param t
    * @tparam T
    * @return
    */

  def transitiveClosure[T](f: T => Stream[T])(t: T): Stream[T] = {
    def maxDepth = 50

    @tailrec def loop(depth: Int, s: Stream[T], maxSeen: Int, totalVisited: Int): Stream[T] = {
      if (s.isEmpty) {
        println(s"Transitive Closure Max stream size = $maxSeen, depth = $depth, total=$totalVisited")
        s
      }
      else if (depth > maxDepth) {
        println(s"Transitive Closure Max stream size = $maxSeen, depth = $depth, total=$totalVisited")
        throw new RuntimeException("too deep")
      }
      else {
        val inSize = s.size
        val nextIteration = s.flatMap(f)
        val nextMaxSeen = Math.max(maxSeen, inSize)
        val nextTotalVisited = totalVisited + inSize
        loop(depth + 1, nextIteration, nextMaxSeen, nextTotalVisited)
      }
    }

    loop(0, f(t), 1, 0)
  }

  /** Recursive iterator of shrink(t) until shrink stream is empty.
    * Each iteration starts with a T value, computes the shrink stream, and then
    * picks one random element of the shrink stream as the input to the next iteration.
    *
    * @param f
    * @param t
    * @tparam T
    * @return
    */
  def randomPath[T](f: T => Stream[T])(t: T): Stream[T] = {
    def maxDepth = 50

    @tailrec def loop(depth: Int, s: Stream[T], maxSeen: Int, totalVisited: Int): Stream[T] = {
      if (s.isEmpty) {
        println(s"Head Path Max stream size = $maxSeen, depth = $depth, total=$totalVisited")
        s
      }
      else if (depth > maxDepth) {
        println(s"Head Path Max stream size = $maxSeen, depth = $depth, total=$totalVisited")
        throw new RuntimeException("too deep")
      }
      else {
        val pick = scala.util.Random.nextInt(s.size)
        val nextIteration = f(s.drop(pick).head)
        val nextSize = nextIteration.size
        val nextMaxSeen = Math.max(maxSeen, s.size)
        loop(depth + 1, nextIteration, nextMaxSeen, totalVisited + s.size)
      }
    }

    loop(0, f(t), 0, 0)
  }

  property("transitive closure of shrink converges to zero") = {
    transitiveClosure(Shrink.shrink[List[Int]])((1 to 4).toList)
    true
  }

  property("random path of shrink converges to zero") = {
    randomPath(Shrink.shrink[List[Int]])((1 to 9).toList)
    true
  }


}
