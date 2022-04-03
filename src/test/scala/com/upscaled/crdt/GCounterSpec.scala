package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GCounterSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A counter" should "count values per replica" in {
    val c1 = new GCounter[String]()
    c1.inc("1").value should equal(1)
  }

  it should "merge with another" in {
    val c1 = new GCounter[String]().inc("1").inc("1")
    val c2 = new GCounter[String]().inc("2").inc("1")

    val merged = c1.merge(c2)
    merged.value should equal(3)
  }

  it should behave like lawfulCRDT[GCounter[String]](
    () => GCounter.empty[String],
    cnt => {
      val replica = Gen.oneOf(Seq("1", "2", "3")).sample.get
      cnt.inc(replica)
    }
  )

}
