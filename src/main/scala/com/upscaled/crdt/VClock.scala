package com.upscaled.crdt

object VClock {
  def empty[ReplicaId] = new VClock[String]()

}

class VClock[ReplicaId] private[crdt] (
    private val cnt: GCounter[ReplicaId] = GCounter.empty[ReplicaId]
) {
  def inc(replicaId: ReplicaId): VClock[ReplicaId] = new VClock(
    cnt.inc(replicaId)
  )
  def merge(v: VClock[ReplicaId]): VClock[ReplicaId] = new VClock(
    cnt.merge(v.cnt)
  )
  def compare(other: VClock[ReplicaId]): Ord = {
    val keys: Set[ReplicaId] =
      (cnt.underlying.keys ++ other.cnt.underlying.keys).toSet
    keys.foldLeft(Ord.Eq){
      case (Ord.Eq, k) if cnt.underlying.getOrElse(k, Zero) > other.cnt.underlying.getOrElse(k, Zero) => Ord.Gt
      case (Ord.Eq, k) if cnt.underlying.getOrElse(k, Zero) < other.cnt.underlying.getOrElse(k, Zero) => Ord.Lt
      case (Ord.Lt, k) if cnt.underlying.getOrElse(k, Zero) > other.cnt.underlying.getOrElse(k, Zero) => Ord.Cc
      case (Ord.Gt, k) if cnt.underlying.getOrElse(k, Zero) < other.cnt.underlying.getOrElse(k, Zero) => Ord.Cc
      case (ord, _) =>  ord
    }
  }
}
