package com.upscaled.crdt

trait CRDT[T <: CRDT[T]] {
  def merge(other: T): T
}
