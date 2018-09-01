package pford19.checkx

import org.scalacheck.Prop.{all, collect, exists, forAll}
import org.scalacheck.{Gen, Properties}

object ExistsBehavior extends Properties("exists behavior") {

  val p = exists(Gen.oneOf(3 to 10) ){ n => collect(n, n==5) { n == 5 } }
  val g = forAll(Gen.oneOf(3 to 10) ){ n =>  collect(n) { n != 5 }}
  property("all positive") = g
  property("all positive 2x") = all(g,g)
  property("all positive 3x") = all(g,g,g)
  property("all positive 4x") = all(g,g,g,g)
  property("5 exists once") = all(p)
  property("5 exists 2x") = all(p,p)
  property("5 exists 3x") = all(p,p,p)
  property("5 exists 4x") = all(p,p,p,p)

}
