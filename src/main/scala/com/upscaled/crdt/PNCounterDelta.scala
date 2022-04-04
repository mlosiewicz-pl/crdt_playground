package com.upscaled.crdt

object PNCounterDelta {
  def empty[ReplicaId] = new PNCounterDelta[ReplicaId]()
}

case class PNCounterDelta[ReplicaId] private[crdt] (
    private val increments: GCounterDelta[ReplicaId] = GCounterDelta.empty[ReplicaId],
    private val decrements: GCounterDelta[ReplicaId] = GCounterDelta.empty[ReplicaId]
) extends CRDT[PNCounterDelta[ReplicaId]] {
  def value = increments.value - decrements.value
  def inc(replicaId: ReplicaId) =
    new PNCounterDelta(increments.inc(replicaId), decrements)
  def dec(replicaId: ReplicaId) =
    new PNCounterDelta(increments, decrements.inc(replicaId))
  def merge(c: PNCounterDelta[ReplicaId]): PNCounterDelta[ReplicaId] = {
    new PNCounterDelta[ReplicaId](
      increments.merge(c.increments),
      decrements.merge(c.decrements)
    )
  }
  def split: (PNCounterDelta[ReplicaId], PNCounterDelta[ReplicaId]) = {
    val (inc, incD) = increments.split
    val (dec, decD) = decrements.split
    PNCounterDelta(inc, dec) -> PNCounterDelta(incD, decD)
  }
}
