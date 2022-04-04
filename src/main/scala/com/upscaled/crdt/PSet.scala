package com.upscaled.crdt

object PSet {
  def empty[T] = new PSet[T]()
}

final case class PSet[T] private[crdt] (
    private[crdt] val adds: GSet[T] = GSet.empty[T],
    private[crdt] val removes: GSet[T] = GSet.empty[T]
) extends CRDT[PSet[T]] {

  def value: Set[T] = adds.underlying -- removes.underlying

  def add(v: T): PSet[T] = PSet(adds.add(v), removes)
  def rem(v: T): PSet[T] = PSet(adds, removes.add(v))

  def merge(gc: PSet[T]): PSet[T] = PSet(this.adds.merge(gc.adds), this.removes.merge(gc.removes))
}
