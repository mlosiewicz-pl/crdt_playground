package com.upscaled.crdt

object ORSet {
  def empty[T, ReplicaId] = new ORSet[T, ReplicaId]()
}

//Add-Wins Observed Remove Set
final case class ORSet[T, R] private[crdt] (
    private[crdt] val adds: Map[T, VClock[R]] = Map.empty[T, VClock[R]],
    private[crdt] val removals: Map[T, VClock[R]] = Map.empty[T, VClock[R]]
) extends CRDT[ORSet[T, R]] {

  def value: Set[T] = adds
    .filter { case (v, clock) =>
      removals.get(v) match {
        case None         => true
        case Some(rClock) => clock.compare(rClock) != Ord.Lt
      }
    }
    .keys
    .toSet

  def add(v: T, replicaId: R): ORSet[T, R] =
    (adds.get(v), removals.get(v)) match {
      case (Some(c), _) => ORSet(adds + (v -> c.inc(replicaId)), removals - v)
      case (_, Some(c)) => ORSet(adds + (v -> c.inc(replicaId)), removals - v)
      case (_, _)       => ORSet(adds + (v -> VClock.empty.inc(replicaId)), removals)
    }

  def rem(v: T, replicaId: R): ORSet[T, R] =
    (adds.get(v), removals.get(v)) match {
      case (Some(c), _) => ORSet(adds - v, removals + (v -> c.inc(replicaId)))
      case (_, Some(c)) => ORSet(adds - v, removals + (v -> c.inc(replicaId)))
      case (_, _)       => ORSet(adds, removals + (v -> VClock.empty.inc(replicaId)))
    }

  override def merge(other: ORSet[T, R]): ORSet[T, R] = {
    val mergedAdds: Map[T, VClock[R]] = (adds.toList ++ other.adds.toList).groupBy(_._1).map {
      case (v, c1 :: c2 :: Nil) => v -> (c1._2.merge(c2._2))
      case (v, c :: Nil)        => v -> c._2
    }
    val mergedRemovals: Map[T, VClock[R]] = (removals.toList ++ other.removals.toList).groupBy(_._1).map {
      case (v, c1 :: c2 :: Nil) => v -> (c1._2.merge(c2._2))
      case (v, c :: Nil)        => v -> c._2
    }
    val newAdds = mergedAdds.filter { case (v, c) =>
      mergedRemovals.get(v) match {
        case None           => true
        case Some(remClock) => c.compare(remClock) != Ord.Lt
      }
    }
    val newRemovals = mergedRemovals.filter { case (v, c) =>
      mergedAdds.get(v) match {
        case None            => true
        case Some(addsClock) => addsClock.compare(c) == Ord.Lt
      }
    }
    ORSet(newAdds, newRemovals)
  }
}
