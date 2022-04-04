package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PNCounterDeltaSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A counter" should "honour increments and decrements" in {
    val c1 = PNCounterDelta.empty[String]
    c1.inc("1").dec("2").value should equal(0)
  }

  it should "merge with another" in {
    val c1 = PNCounterDelta.empty[String].inc("1").inc("1")
    val c2 = PNCounterDelta.empty[String].inc("2").inc("1").dec("2")

    val merged = c1.merge(c2)
    merged.value should equal(2)
  }

  it should behave like lawfulCRDT[PNCounterDelta[String]](
    () => PNCounterDelta.empty[String],
    for {
      replica <- Gen.oneOf(Seq("1", "2", "3"))
      op <- Gen
        .oneOf[(PNCounterDelta[String], String) => PNCounterDelta[String]](
          Seq(
            (c: PNCounterDelta[String], r: String) => c.inc(r),
            (c: PNCounterDelta[String], r: String) => c.dec(r)
          )
        )
    } yield cnt => op(cnt, replica)
  )

}
