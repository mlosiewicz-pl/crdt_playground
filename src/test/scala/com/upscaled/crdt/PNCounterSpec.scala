package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PNCounterSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A counter" should "honour incremenets and decrements" in {
    val c1 = PNCounter.empty[String]
    c1.inc("1").dec("2").value should equal(0)
  }

  it should "merge with another" in {
    val c1 = PNCounter.empty[String].inc("1").inc("1")
    val c2 = PNCounter.empty[String].inc("2").inc("1").dec("2")

    val merged = c1.merge(c2)
    merged.value should equal(2)
  }

  it should behave like lawfulCRDT[PNCounter[String]](
    () => PNCounter.empty[String],
    cnt => {
      val replica = Gen.oneOf(Seq("1", "2", "3")).sample.get
      val op = Gen
        .oneOf[(PNCounter[String], String) => PNCounter[String]](
          Seq(
            (c: PNCounter[String], r: String) => c.inc(r),
            (c: PNCounter[String], r: String) => c.dec(r)
          )
        )
        .sample
        .get
      op(cnt, replica)
    }
  )

}
