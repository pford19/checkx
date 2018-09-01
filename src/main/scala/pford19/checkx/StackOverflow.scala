package pford19.checkx

import org.scalacheck.Prop.{collect, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}

object StackOverflowExample extends Properties("stackoverflow example") {

  def genSameSizeSets[T, U](gt: Gen[T], gu: Gen[U]): Gen[(Set[T], Set[U])] = {
    for {n <- Gen.posNum[Int]
         ts <- Gen.containerOfN[Set, T](n, gt)
         us <- Gen.containerOfN[Set, U](n, gu)
         minsize = Math.min(ts.size, us.size)
    } yield (ts.take(minsize), us.take(minsize))
  }

  val g = genSameSizeSets(Arbitrary.arbitrary[Int], Arbitrary.arbitrary[Char])

  property("same size") = forAll(g) { case (intSet, charSet) =>
    collect(intSet.size, charSet.size) {
      intSet.size == charSet.size
    }
  }


}
