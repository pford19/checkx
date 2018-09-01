package pford19.checkx

import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Prop

import SomeNewGenerators.streamFromGen

object MoreGenTests extends Properties("infiniteStream") {


  val g = Gen.infiniteStream(Gen.oneOf(1 to 9))

  val s = streamFromGen(g)

  val tx1 = s.head
  val tx2 = s.drop(1).head
  val tx3 = s.drop(2).head
  val tx4 = s.drop(3).head

  println(tx1.take(4).toList)
  println(tx2.take(4).toList)
  println(tx3.take(4).toList)
  println(tx4.take(4).toList)




}
