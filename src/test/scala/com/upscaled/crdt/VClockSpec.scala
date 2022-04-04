package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VClockSpec extends AnyFlatSpec with Matchers with CRDTBehaviors{

  "Vector Clock" should "compare with another" in {
    val v1 = VClock.empty[String].inc("1").inc("2")
    val v2 = VClock.empty[String].inc("1")

    v1.compare(v2) should equal(Ord.Gt)
    v2.compare(v1) should equal(Ord.Lt)
    v2.inc("2").compare(v1) should equal(Ord.Eq)
    v2.inc("1").inc("1").compare(v1) should equal(Ord.Cc)
  }

  it should behave like lawfulCRDT[VClock[String]](
    () => VClock.empty[String],
    Gen.oneOf(Seq("1", "2", "3")).map(r => cnt => cnt.inc(r))
  )

}
