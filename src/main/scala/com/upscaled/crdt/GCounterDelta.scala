package com.upscaled.crdt

import com.upscaled.crdt.{upsert, mergeOption}
import math.Numeric.Implicits.infixNumericOps
import math.Integral.Implicits.infixIntegralOps

object GCounterDelta {
  def empty[ReplicaId] = new GCounterDelta[ReplicaId]()
}

final case class GCounterDelta[ReplicaId] private[crdt] (
    private[crdt] val underlying: Map[ReplicaId, BigInt] = Map.empty[ReplicaId, BigInt],
    private[crdt] val delta: Option[GCounterDelta[ReplicaId]] = None
) extends CRDT[GCounterDelta[ReplicaId]] {

  def value: BigInt = underlying.foldLeft(Zero)(_ + _._2)

  def inc(replicaId: ReplicaId): GCounterDelta[ReplicaId] = {
    val deltaMap: Map[ReplicaId, BigInt]        = delta.map(_.underlying).getOrElse(Map.empty)
    val updatedDeltaMap: Map[ReplicaId, BigInt] = deltaMap.upsert(replicaId, 1, _ + 1)
    GCounterDelta(
      underlying.upsert(replicaId, 1, _ + 1),
      Some(GCounterDelta(updatedDeltaMap, None))
    )
  }

  def merge(gc: GCounterDelta[ReplicaId]): GCounterDelta[ReplicaId] = {
    val values = underlying.foldLeft(gc.underlying) { case (map, (r, v)) =>
      map.upsert(r, v, v.max)
    }
    val newDelta = delta.mergeOption(gc.delta, (c1, c2) => c1.merge(c2))
    new GCounterDelta(values, newDelta)
  }

  def split: (GCounterDelta[ReplicaId], GCounterDelta[ReplicaId]) = GCounterDelta(underlying, None) -> delta.getOrElse(GCounterDelta.empty)

}
