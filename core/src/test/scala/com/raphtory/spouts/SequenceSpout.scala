package com.raphtory.spouts

import com.raphtory.api.input.Spout

/** Output input messages to Raphtory.
  * This spout is mainly useful for testing functionality
  */
class SequenceSpout[T](seq: Seq[T]) extends Spout[T] {
  private val seqIterator                  = seq.iterator
  override def spoutReschedules(): Boolean = false

  override def hasNext: Boolean = seqIterator.hasNext

  override def next(): T = seqIterator.next()
}

object SequenceSpout {
  def apply[T](seq: T*) = new SequenceSpout[T](seq)
}
