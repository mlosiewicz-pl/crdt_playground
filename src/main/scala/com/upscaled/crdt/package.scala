package com.upscaled

package crdt {
  val Zero = BigInt(0)

  enum Ord:
    case Lt, Eq, Gt, Cc
}
