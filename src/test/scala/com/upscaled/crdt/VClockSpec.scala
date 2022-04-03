package com.upscaled.crdt

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VClockSpec extends AnyFlatSpec with Matchers {

  "Vector Clock" should "compare with another" in {
    val v1 = VClock.empty[String].inc("1").inc("2")
    val v2 = VClock.empty[String].inc("1")

    v1.compare(v2) should equal(Ord.Gt)
    v2.compare(v1) should equal(Ord.Lt)
    v2.inc("2").compare(v1) should equal(Ord.Eq)
    v2.inc("1").inc("1").compare(v1) should equal(Ord.Cc)
  }

}
