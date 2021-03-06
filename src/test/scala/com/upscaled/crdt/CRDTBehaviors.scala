package com.upscaled.crdt

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.forAll
import org.scalacheck.*
import org.scalatest.prop.Configuration

trait CRDTBehaviors { this: AnyFlatSpec with Matchers =>

  implicit val generatorDrivenConfig: Configuration.PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSize = 100)

  def lawfulCRDT[T <: CRDT[T]](zero: () => T, op: Gen[T => T]) = {
    implicit val gen: Gen[T] = for {
      i <- Gen.posNum[Int]
      ops: List[T => T] <- Gen.listOfN(i, op)
      crdt = ops.foldRight(zero())(_(_))
    } yield crdt

    implicit val arb: Arbitrary[T] = Arbitrary(gen)

    it should "satisfy (x • y) = (y • x)" in {
      forAll { (x: T, y: T) =>
        x.merge(y) should equal(y.merge(x))
      }
    }

    it should "satisfy associativity (x • y) • z =  x • (y • z)" in {
      forAll { (x: T, y: T, z: T) =>
//        println(
//          s"""
//             |x = $x
//             |y = $y
//             |z = $z
//             |""".stripMargin)
        (x.merge(y)).merge(z) should equal(x.merge((y.merge(z))))
      }
    }

    it should "satisfy idempotency (x • x) =  x " in {
      forAll { (x: T) =>
        x.merge(x) should equal(x)
      }
    }

  }

}
