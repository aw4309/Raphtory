package com.raphtory.internals.communication

private[raphtory] trait Connector {

  def register[T](
      id: String,
      messageHandler: (T, Array[Byte]) => Unit,
      topics: Seq[CanonicalTopic[T]]
  ): CancelableListener

  def endPoint[T](topic: CanonicalTopic[T]): EndPoint[T]

  def shutdown(): Unit
}
