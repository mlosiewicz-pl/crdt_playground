package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GCounterDeltaSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A counter" should "count values per replica" in {
    val c1 = GCounterDelta.empty[String]
    c1.inc("1").value should equal(1)
  }

  it should "merge with another" in {
    val c1 = GCounterDelta.empty[String].inc("1").inc("1")
    val c2 = GCounterDelta.empty[String].inc("2").inc("1")

    val merged = c1.merge(c2)
    merged.value should equal(3)
    c1.merge(c2) should equal(c2.merge(c1))
  }

  it should behave like lawfulCRDT[GCounterDelta[String]](
    () => GCounterDelta.empty[String],
    Gen.oneOf(Seq("1", "2", "3")).map(r => cnt => cnt.inc(r))
  )

}
