package com.upscaled.crdt

object GCounter {
  def empty[ReplicaId] = new GCounter[ReplicaId]()
}

final case class GCounter[ReplicaId] private[crdt] (
    private[crdt] val underlying: Map[ReplicaId, BigInt] = Map.empty[ReplicaId, BigInt]
) extends CRDT[GCounter[ReplicaId]] {

  def value: BigInt = underlying.values.sum

  def inc(replicaId: ReplicaId): GCounter[ReplicaId] = {
    val value: BigInt = underlying.get(replicaId) match {
      case Some(v) => v + 1
      case None    => 1
    }
    new GCounter[ReplicaId](underlying + (replicaId -> value))
  }

  def merge(gc: GCounter[ReplicaId]): GCounter[ReplicaId] = {
    var merged = gc.underlying
    for ((k, v) <- underlying) {
      val otherValue = merged.getOrElse(k, Zero)
      if (v > otherValue)
        merged = merged.updated(k, v)
    }
    new GCounter(merged)
  }
}
