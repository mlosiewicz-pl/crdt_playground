package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GSetSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A set" should "preserve values" in {
    val c1 = GSet.empty[Int]
    c1.add(1).add(2).add(3).value should contain allOf (1, 2, 3)
  }

  it should behave like lawfulCRDT[GSet[Int]](
    () => GSet.empty[Int],
    Gen.posNum[Int].map(v => s => s.add(v))
  )

}
