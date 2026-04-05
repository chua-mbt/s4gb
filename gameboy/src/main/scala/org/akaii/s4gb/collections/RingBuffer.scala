package org.akaii.s4gb.collections

class RingBuffer[A] private (capacity: Int) {
  private val buffer: Array[Option[A]] = Array.fill(capacity)(None)
  private var head: Int = 0
  private var tail: Int = 0
  private var count: Int = 0

  def enqueue(item: A): Unit =
    if (!isFull) {
      buffer(tail) = Some(item)
      tail = (tail + 1) % capacity
      count += 1
    }

  def dequeue(): A =
    if (isEmpty) throw new NoSuchElementException("Buffer is empty")
    val item = buffer(head).get
    buffer(head) = None
    head = (head + 1) % capacity
    count -= 1
    item

  def clear(): Unit = {
    head = 0
    tail = 0
    count = 0
  }

  def peek(): A =
    if (isEmpty) throw new NoSuchElementException("Buffer is empty")
    buffer(head).get

  def isFull: Boolean = count == capacity

  def isEmpty: Boolean = count == 0

  def size: Int = count
}

object RingBuffer {
  def apply[A](capacity: Int): RingBuffer[A] =
    if (capacity <= 0) throw new IllegalArgumentException("Capacity must be positive")
    else new RingBuffer[A](capacity)
}
