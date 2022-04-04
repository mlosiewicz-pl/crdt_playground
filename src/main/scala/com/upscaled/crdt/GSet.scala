package com.upscaled.crdt

object GSet {
  def empty[T] = new GSet[T]()
}

final case class GSet[T] private[crdt] (
    private[crdt] val underlying: Set[T] = Set.empty[T]
) extends CRDT[GSet[T]] {

  def value: Set[T] = underlying

  def add(v: T): GSet[T] = GSet(underlying + v)

  def merge(gc: GSet[T]): GSet[T] = GSet(this.underlying ++ gc.underlying)
}
