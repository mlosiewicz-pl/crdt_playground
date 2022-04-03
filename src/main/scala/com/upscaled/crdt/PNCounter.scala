package com.upscaled.crdt

object PNCounter {
  def empty[ReplicaId] = new PNCounter[ReplicaId]()
}

case class PNCounter[ReplicaId] private[crdt] (
    private val increments: GCounter[ReplicaId] = GCounter.empty[ReplicaId],
    private val decrements: GCounter[ReplicaId] = GCounter.empty[ReplicaId]
) extends CRDT[PNCounter[ReplicaId]] {
  def value = increments.value - decrements.value
  def inc(replicaId: ReplicaId) =
    new PNCounter(increments.inc(replicaId), decrements)
  def dec(replicaId: ReplicaId) =
    new PNCounter(increments, decrements.inc(replicaId))
  def merge(c: PNCounter[ReplicaId]): PNCounter[ReplicaId] = {
    new PNCounter[ReplicaId](
      increments.merge(c.increments),
      decrements.merge(c.decrements)
    )
  }
}
