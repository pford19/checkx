package pford19.checkx

import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Random

object RandomSpec extends Properties("random") {

  def r(n: Option[Long] = None) = n match {
    case Some(seed) => new Random(seed)
    case None => new Random()
  }

  def rstream(r: Random): Stream[Long] = Stream.cons(r.nextLong, rstream(r))

  def rtake(seed: Option[Long] = None)(n: Int): Stream[Long] = rstream(r(seed)).take(n)

  property("same seeds => different instances")    = Prop.forAll(Gen.choose(1, 1000)) { n => r(Some(n)) ne r(Some(n))                        }
  property("same seeds => equal streams")           = Prop.forAll(Gen.choose(1, 1000)) { n => rtake(Some(n))(200) == rtake(Some(n))(200)      }
  property("different seeds => unequal  streams") = Prop.forAll(Gen.choose(1, 1000)) { n => rtake(Some(n))( 200) != rtake(Some(n+1))( 200)    }
  property("randome seeds => different streams")   = Prop.forAll(Gen.choose(1, 1000)) { n => rtake()(200) != rtake()(200)                    }

}
