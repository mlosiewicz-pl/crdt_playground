package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PSetSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A set" should "preserve values" in {
    val c1 = PSet.empty[Int]
    c1.add(1).add(2).add(3).rem(2).value should contain allOf (1, 3)
  }

  it should behave like lawfulCRDT[PSet[Int]](
    () => PSet.empty[Int],
    for {
      v <- Gen.choose[Int](1, 10)
      op <- Gen.oneOf[(PSet[Int], Int) => PSet[Int]](
        Seq(
          (s: PSet[Int], v: Int) => s.add(v),
          (s, v) => s.rem(v)
        )
      )
    } yield s => op(s, v)
  )

}
