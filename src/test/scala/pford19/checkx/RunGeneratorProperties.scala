package pford19.checkx

import org.scalacheck.{Gen, Properties}
import SomeNewGenerators._
import org.scalacheck.Prop._

object RunGeneratorProperties extends Properties("runs") {

  val groupSize = 3

  val g: Gen[List[Int]] = genGroups(groupSize, Gen.choose(0, 9))

  property("right size") = forAll(g) (group => group.size == groupSize)

}
