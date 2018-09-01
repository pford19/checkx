package pford19.checkx

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Gen.frequency
//import junit.framework.TestCase
import org.scalacheck.Gen.listOf
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers.check
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed

import scala.annotation.tailrec


class GenTest extends FlatSpec {

  implicit object IntIsIntegral

  def streamOf[T](gen: Gen[T]): Stream[T] = Stream.cons(gen.sample.get, streamOf(gen))

  def printSamples[T](g: Gen[T], n: Int): Boolean = {
    for (_ <- 1 to n) println(g.sample.get)
    true
  }

  "generators" must "do interesting things" in {
    check {
      forAll(Gen.asciiPrintableChar) { c => c.toInt >= ' '.toInt && c.toInt < 0x7F }
    }
    check {
      forAll(Gen.alphaChar) { c => c != '1' }
    }

    check {
      forAll(Gen.alphaLowerChar) { c => c != 'A' }
    }

    check {
      forAll(Gen.numChar) { c => c != 'A' }
    }
    check {
      forAll(Gen.atLeastOne(List(1, 2, 3))) { ns => ns.size >= 1 && ns.size <= 3 }
    }

    check {
      forAll(Gen.atLeastOne(Gen.alphaChar, Gen.numChar)) { cs => cs.size >= 1 && cs.size <= 2 }
    }

    check {
      forAll(Gen.choose(3, 10)) { n => n >= 3 && n <= 10 }
    }

    check {
      forAll(Gen.const(1)) { n => n == 1 }
    }

    check {
      val g = Gen.listOf(arbitrary[Int])
      forAll(g) { xs => xs.size >= 0 }
    }
    check {
      val g = arbitrary[Int]
      forAll(g) {
        n => n >= Int.MinValue
      }
    }

    check {
      forAll(Arbitrary.arbitrary[Boolean]) { b: Boolean => b || !b }
    }

    check {
      forAll(Arbitrary.arbitrary[Float]) { f => Math.abs(f) >= 0.0 }
    }
    // pick: fixed length, not in order, no repetition
    check {
      forAll(Gen.pick(5, 1 to 10)) { ns => ns.size == 5 && ns.toSet.size == ns.size }
    }
    // choose: random length, no repetition
    check {
      forAll(Gen.someOf(1 to 10)) { ns => ns.size <= 10 && ns.toSet.size == ns.size }
    }

    check {
      forAll(SomeNewGenerators.genPartitionsOf(10, 5)) { p => p.sum == 10 && p.max <= 5 }
    }


  }


  "partitons" must "look interesting" in {
    printSamples(SomeNewGenerators.genPartitionsOf(10, 10), 3)
    printSamples(SomeNewGenerators.genPartitionsOf(10, 5), 3)
    printSamples(SomeNewGenerators.genPartitionsOf(10, 3), 3)
  }

  "arbitrary ints" must "arbitrary" in {
    printSamples(arbitrary[Int], 3)
  }
  "arbitrary floats" must "be arbitrary" in {
    printSamples(arbitrary[Float], 3)
  }
  "arbitrary strings" must "be arbitrary" in {
    printSamples(arbitrary[String], 3)
  }
  "arbitrary chars" must "be arbitrary" in {
    printSamples(arbitrary[Char], 3)
  }

  "permutations" must "generate 75% of permutations" in {
    //printSamples(DSLGenerators.genPermutation(4), 20)
    //    val ps = for(_ <- 1 to 100) yield g.sample.get
    //    val grouped = ps.groupBy(p => p).toList.map{ case (k, v) => (v.size, k)}.sortBy{ case (n, k) => k.toString}
    //    println(s"distinct permutations=${grouped.size}")
    //    grouped.map(println)

    //SomeNewGenerators.streamFromGen(SomeNewGenerators.genPermutation(4)).take(50).foreach(println(_))

    check {
      forAll(SomeNewGenerators.genPermutation(4)) { ps =>
        (ps.size == 4)
      }
    }
    check {
      forAll(SomeNewGenerators.genPermutation(4)) { ps =>
        (ps.toSet == (0 to 3).toSet)
      }
    }
    check {
      forAll(SomeNewGenerators.genPermutation(5)) { ps =>
        (ps.size == 5) && (ps.toSet == (0 to 4).toSet)
      }
    }

    val g = SomeNewGenerators.genPermutation(4)
    val ps = for (_ <- 1 to 24 * 3) yield g.sample.get
    assert(ps.distinct.size / 24.0 > 0.75) // at least 85% of the permutations

    val g5 = SomeNewGenerators.genPermutation(5)
    val ps5 = for (_ <- 1 to 120 * 3) yield g5.sample.get
    assert(ps5.distinct.size / 120.0 > 0.75) // at least 85% of the permutations
  }

  "choose" must "be be reasonably uniform" in {
    val g = Gen.choose(0, 99)
    val u = for (_ <- 1 to 200) yield g.sample.get
    val buckets = u.map(_ / 10).groupBy(n => n).map { case (n, v) => (n, v.size) }.toList.sorted
    //println(buckets)
    buckets.map(_._2).min > 10 &&
      buckets.map(_._2).max < 30
  }


  "generators" must "expose parameters" in {
    val g = Gen.oneOf(List(1, 2, 3))
    val g1 = g.withPerturb(s => s.slide)
    val g2 = g.withPerturb(s => s.reseed(1023))
  }

  "size" must "work" in {
    val size = Gen.size.sample.get

    println(s"Current size = $size")
    assert(size > 0)
  }

  "resize" must "work" in {
    val currentSize = Gen.size.sample.get
    val g = Gen.listOf(1 to 99)
    val h = Gen.resize(currentSize / 2, g)

    val gmax = (for (_ <- 1 to 100) yield g.sample.get.size).max
    val hmax = (for (_ <- 1 to 100) yield h.sample.get.size).max
    assert(gmax <= currentSize)
    assert(gmax > currentSize / 2)
    assert(hmax <= currentSize / 2)

  }

  "generators" must "act like streams" in {

    val g = Gen.oneOf(-9 to 9)
    val sg = streamOf(g)
    println(sg.take(50).toList)
  }




}
