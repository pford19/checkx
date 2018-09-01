package pford19.checkx

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{collect, exists, forAll}
import org.scalacheck.Prop._


object CombinatoricProperties extends Properties("Combinatorics") {
  import Combinatorics._

  property("0! = 1") = factorial(0) == 1.0
  property("1! = 1") = factorial(1) == 1.0
  property("5! = 120") = factorial(5) == 120.0

  property("170! is not Infinity") = {val n = 170 ; val big = factorial(n); !big.isInfinity }
  property("171! is Infinity") = {val n = 171 ; val big = factorial(n); big.isInfinity }

  property("sigma (n choose i) = 2^n") = (0 to 10).forall(n =>  (0 to n).map(combinationCount(_, n)).sum == Math.pow(2, n))


  property("permuationCount(n, n) == n!") = (0 to 50).forall(n => factorial(n) == permutationCount(n, n))

  property("permutationCount(3, 10) == 720") = permutationCount(3, 10) == 720.0

  property("combinationCount(3, 10) == 120") = combinationCount(3, 10) == 120.0

  property("combinationCount(0, n) == 1") = forAll  (Gen.choose(1, 250)) { n: Int => (n > 170) || combinationCount(0, n) == 1 }
  property("combinationCount(1, n) == n") = forAll  (Gen.choose(1, 250)) { n: Int => (n > 170) || combinationCount(1, n) == n }
  property("combinationCount(n-1, n) == n") = forAll(Gen.choose(1, 250)) { n: Int => (n > 170) || combinationCount(n-1, n) == n }
  property("combinationCount(n, n) == 1") = forAll  (Gen.choose(1, 250)) { n: Int => (n > 170) || combinationCount(n, n) == 1 }

  property("longFactorial bounds") = {
    val x = (1 to 100).find(longFactorial(_).isEmpty).get
    println(s"longFactorial ${x-1}!=${longFactorial(x-1)}, ${x}!=${longFactorial(x)}")
    longFactorial(x-1).isDefined  &&
    longFactorial(x).isEmpty
  }
  }
