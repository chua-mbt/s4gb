package org.akaii.s4gb.collections

import scala.reflect.ClassTag

class RingBuffer[A] private (capacity: Int)(using ClassTag[A], Preallocated[A]) {
  private val buffer: Array[A] = Array.fill(capacity)(summon[Preallocated[A]].allocate)
  private val occupied: Array[Boolean] = Array.fill(capacity)(false)
  private var head: Int = 0
  private var tail: Int = 0
  private var count: Int = 0

  def enqueue(item: A): Unit =
    if (!isFull) {
      summon[Preallocated[A]].copyInto(buffer(tail), item)
      occupied(tail) = true
      tail = (tail + 1) % capacity
      count += 1
    }

  def enqueueAll(items: Iterable[A]): Unit =
    items.foreach(enqueue)

  def dequeue(into: Array[A]): Int = {
    val toCopy = math.min(count, into.length)
    for (i <- 0 until toCopy) {
      summon[Preallocated[A]].copyInto(into(i), buffer(head))
      occupied(head) = false
      head = (head + 1) % capacity
      count -= 1
    }
    toCopy
  }

  def clear(): Unit = {
    head = 0
    tail = 0
    count = 0
    for (i <- 0 until capacity) occupied(i) = false
  }

  def isFull: Boolean = count == capacity

  def isEmpty: Boolean = count == 0

  def size: Int = count
}

object RingBuffer {
  def apply[A](capacity: Int)(using ct: ClassTag[A], p: Preallocated[A]): RingBuffer[A] =
    if (capacity <= 0) throw new IllegalArgumentException("Capacity must be positive")
    else new RingBuffer[A](capacity)
}
