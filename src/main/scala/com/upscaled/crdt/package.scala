package com.upscaled

package crdt {
  val Zero = BigInt(0)

  enum Ord:
    case Lt, Eq, Gt, Cc

  extension [K,V](m: Map[K, V]){
    def upsert(k: K, v: V, f: V => V): Map[K,V] = {
      m.get(k) match
        case None => m + (k -> v)
        case Some(vv) => m + (k -> f(vv))
    }
  }
  
  extension [T](opt: Option[T]) {
    def mergeOption(other: Option[T], f: (T, T) => T) = {
      (opt, other) match
        case (Some(x), Some(y)) => Some(f(x,y))
        case (Some(x), None) => Some(x)
        case (None, Some(y)) => Some(y)
        case (None, None) => None
    }
  }
}
