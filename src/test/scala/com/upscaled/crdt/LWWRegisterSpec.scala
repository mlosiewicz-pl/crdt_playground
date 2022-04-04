package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDateTime, ZoneOffset}

class LWWRegisterSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A register" should "store the latest write" in {
    val reg = LWWRegister[Int](1, LocalDateTime.now())
    reg.set(2, LocalDateTime.now().minusMinutes(1)).value should equal(1)
  }

  //flaky with two registries having the same time
  it should behave like lawfulCRDT[LWWRegister[Int]](
    () => LWWRegister(0),
    for {
      seconds <- Gen.chooseNum(LocalDateTime.MIN.toEpochSecond(ZoneOffset.UTC), LocalDateTime.MAX.toEpochSecond(ZoneOffset.UTC))
      nanos   <- Gen.chooseNum(LocalDateTime.MIN.getNano, LocalDateTime.MAX.getNano)
      date = LocalDateTime.ofEpochSecond(seconds, nanos, ZoneOffset.UTC)
      v <- Gen.posNum[Int]
    } yield reg => reg.set(v, date)
  )

}
