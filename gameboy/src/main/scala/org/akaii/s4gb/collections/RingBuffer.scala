package org.akaii.s4gb.collections

import scala.reflect.ClassTag

class RingBuffer[A] private (capacity: Int)(using ClassTag[A], Preallocated[A]) {
  private val buffer: Array[A] = Array.fill(capacity)(summon[Preallocated[A]].allocate)
  private var head: Int = 0
  private var tail: Int = 0
  private var count: Int = 0

  def enqueue(item: A): Unit =
    if (!isFull) {
      summon[Preallocated[A]].copyInto(buffer(tail), item)
      tail = (tail + 1) % capacity
      count += 1
    }

  def enqueueAll(items: Iterable[A]): Unit =
    items.foreach(enqueue)

  def dequeue(into: Array[A]): Int = {
    val toCopy = math.min(count, into.length)
    for (i <- 0 until toCopy) {
      summon[Preallocated[A]].copyInto(into(i), buffer(head))
      head = (head + 1) % capacity
      count -= 1
    }
    toCopy
  }

  def pop(into: A): Boolean = {
    if (isEmpty) false
    else {
      summon[Preallocated[A]].copyInto(into, buffer(head))
      head = (head + 1) % capacity
      count -= 1
      true
    }
  }

  def clear(): Unit = {
    head = 0
    tail = 0
    count = 0
  }

  /**
   * Walk every cell from the head around to the head again and let `fill` decide
   * what each one holds. The `live` flag separates cells inside the current window
   * from consumed ones, whose value is stale. Size becomes capacity, so consumed
   * cells never block a write.
   */
  def fillFromHead(fill: (A, Boolean, Int) => A): Unit = {
    val live = count
    var i = 0
    while (i < capacity) {
      val index = (head + i) % capacity
      summon[Preallocated[A]].copyInto(buffer(index), fill(buffer(index), i < live, i))
      i += 1
    }
    count = capacity
    tail = head
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
