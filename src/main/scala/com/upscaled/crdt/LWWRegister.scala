package com.upscaled.crdt

import com.upscaled.crdt

import java.time.LocalDateTime


case class LWWRegister[T] (value: T , timestamp: LocalDateTime = LocalDateTime.MIN) extends CRDT[LWWRegister[T]] {
  def set(v: T, t: LocalDateTime): LWWRegister[T] = {
      if t.isAfter(timestamp) then
        LWWRegister(v, t)
      else
        this
  }

  override def merge(other: LWWRegister[T]): LWWRegister[T] = {
    if other.timestamp isAfter(timestamp) then
      other
    else 
      this
  }
}
