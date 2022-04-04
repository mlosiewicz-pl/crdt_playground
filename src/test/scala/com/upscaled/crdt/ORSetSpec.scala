package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ORSetSpec extends AnyFlatSpec with Matchers with CRDTBehaviors {

  "A set" should "preserve values" in {
    val c1 = ORSet.empty[Int, String]
    c1.add(1, "1").add(2, "1").rem(4, "1").add(3, "2").value should contain allOf (1, 2, 3)
  }

  it should "merge with other set" in {
    val s1 = ORSet.empty[Int, String].add(1, "1").add(1, "2").add(2, "3")
    val s2 = ORSet.empty[Int, String].add(1, "1").rem(1, "1")

    s1.merge(s2).value should contain allOf (1, 2)
  }

  it should "merge with other set commutatively #1" in {
    val x = ORSet.empty[Int, String].add(8, "2").add(7, "3").add(4, "3").add(4, "3").rem(10, "2").rem(5, "1")
    val y = ORSet.empty[Int, String].add(7, "2").add(7, "1").add(8, "1").add(2, "3").rem(3, "1").rem(1, "1").rem(10, "3")
    val z = ORSet.empty[Int, String].add(7, "1").add(10, "2").add(10, "1").rem(3, "1").rem(1, "3").rem(5, "3").rem(5, "2")

    val xy   = x.merge(y)
    val xyz  = xy.merge(z)
    val yz   = y.merge(z)
    val xyz2 = x.merge(yz)

    xyz.value should equal(xyz2.value)
  }

  it should "merge with other set commutatively #2" in {
    val x = ORSet.empty[Int, String]
      .add(5,"2")
      .rem(10,"3")
      .rem(3,"1")
      .rem(10,"1")
      .add(1,"2")
      .rem(1,"2")
      .rem(6,"2")
      .rem(4,"1")
      .add(5,"3")
      .add(3,"1")
      .rem(5,"3")
      .add(2,"2")
      .add(1,"1")
      .rem(4,"3")
      .add(8,"1")
      .rem(7,"1")
      .add(3,"1")
      .rem(1,"3")
      .rem(3,"2")
    val y = ORSet.empty[Int, String]
      .rem(5,"2")
      .rem(9,"2")
      .rem(9,"1")
      .add(7,"2")
      .add(10,"2")
      .add(10,"2")
      .add(5,"2")
      .add(3,"2")
      .add(8,"2")
      .rem(3,"2")
      .add(9,"2")
      .rem(1,"1")
      .add(10,"2")
      .rem(6,"3")
      .add(7,"3")
      .add(1,"2")
      .rem(3,"2")
      .rem(2,"2")
      .add(7,"1")
      .add(10,"2")
      .add(7,"2")
      .add(1,"2")
      .add(10,"3")
      .rem(7,"2")
      .rem(6,"2")
      .add(4,"3")
      .add(2,"1")
    val z = ORSet.empty[Int, String]
      .rem(6,"1")
      .add(3,"2")
      .add(8,"3")
      .add(6,"1")
      .rem(1,"3")
      .rem(7,"2")
      .rem(9,"1")
      .rem(3,"3")
      .add(2,"1")
      .add(1,"3")
      .rem(1,"1")
      .rem(5,"1")
      .add(4,"3")
      .rem(3,"2")
      .rem(9,"1")
      .rem(2,"2")
      .add(6,"3")
      .rem(1,"1")

    val xy   = x.merge(y)
    val xyz  = xy.merge(z)
    val yz   = y.merge(z)
    val xyz2 = x.merge(yz)

    xyz.value should equal(xyz2.value)
    xyz should equal(xyz2)
  }

  it should behave like lawfulCRDT[ORSet[Int, String]](
    () => {
//      println("ORSet.empty[Int, String]")
      ORSet.empty[Int, String]
    },
    for {
      r <- Gen.oneOf("1", "2", "3")
      v <- Gen.choose[Int](1, 10)
      op <- Gen.oneOf[(ORSet[Int, String], Int, String) => ORSet[Int, String]](
        Seq(
          (s: ORSet[Int, String], v: Int, r: String) => {
//            println(s".add($v,\"$r\")")
            s.add(v, r)
          },
          (s, v, r) => {
//            println(s".rem($v,\"$r\")")
            s.rem(v, r)
          }
        )
      )
    } yield s => op(s, v, r)
  )

}
