package com.upscaled.crdt

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PNCounterSpec extends AnyFlatSpec with Matchers {

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

}
