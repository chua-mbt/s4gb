package org.akaii.s4gb.collections

import munit.FunSuite

class RingBufferTests extends FunSuite {

  test("isEmpty on creation") {
    val buffer = RingBuffer[Int](3)
    assert(buffer.isEmpty)
    assert(!buffer.isFull)
    assertEquals(buffer.size, 0)
  }

  test("enqueue and dequeue - single element") {
    val buffer = RingBuffer[String](3)
    buffer.enqueue("a")

    assert(!buffer.isEmpty)
    assertEquals(buffer.size, 1)

    assertEquals(buffer.dequeue(), "a")
    assert(buffer.isEmpty)
    assertEquals(buffer.size, 0)
  }

  test("enqueue and dequeue - multiple elements in order") {
    val buffer = RingBuffer[Int](5)
    buffer.enqueue(1)
    buffer.enqueue(2)
    buffer.enqueue(3)

    assertEquals(buffer.dequeue(), 1)
    assertEquals(buffer.dequeue(), 2)
    assertEquals(buffer.dequeue(), 3)
    assert(buffer.isEmpty)
  }

  test("clear empties the buffer") {
    val buffer = RingBuffer[Int](4)

    buffer.enqueue(1)
    buffer.enqueue(2)
    buffer.clear()

    assert(buffer.isEmpty)
    assertEquals(buffer.size, 0)
  }

  test("clear allows reuse after wrap around") {
    val buffer = RingBuffer[Int](4)

    buffer.enqueue(1)
    buffer.enqueue(2)
    buffer.dequeue()
    buffer.enqueue(3)

    buffer.clear()

    buffer.enqueue(4)
    buffer.enqueue(5)

    assertEquals(buffer.dequeue(), 4)
    assertEquals(buffer.dequeue(), 5)
  }

  test("peek returns oldest without removing") {
    val buffer = RingBuffer[Char](4)
    buffer.enqueue('x')
    buffer.enqueue('y')

    assertEquals(buffer.peek(), 'x')
    assertEquals(buffer.peek(), 'x')
    assertEquals(buffer.size, 2)
    assert(!buffer.isEmpty)
  }

  test("dequeue on empty throws NoSuchElementException") {
    val buffer = RingBuffer[Int](3)
    val ex = intercept[NoSuchElementException] {
      buffer.dequeue()
    }
    assertEquals(ex.getMessage, "Buffer is empty")
  }

  test("dequeue after clear throws NoSuchElementException") {
    val buffer = RingBuffer[Int](2)

    buffer.enqueue(1)
    buffer.clear()

    intercept[NoSuchElementException] {
      buffer.dequeue()
    }
  }

  test("peek on empty throws NoSuchElementException") {
    val buffer = RingBuffer[String](3)
    val ex = intercept[NoSuchElementException] {
      buffer.peek()
    }
    assertEquals(ex.getMessage, "Buffer is empty")
  }

  test("isFull when at capacity") {
    val buffer = RingBuffer[Int](2)
    buffer.enqueue(1)
    buffer.enqueue(2)

    assert(buffer.isFull)
    assertEquals(buffer.size, 2)
  }

  test("enqueue on full does not overwrite") {
    val buffer = RingBuffer[Int](2)
    buffer.enqueue(1)
    buffer.enqueue(2)
    buffer.enqueue(3)

    assertEquals(buffer.dequeue(), 1)
    assertEquals(buffer.dequeue(), 2)
  }

  test("wrap around - head and tail wrap") {
    val buffer = RingBuffer[Int](4)

    buffer.enqueue(1)
    buffer.enqueue(2)
    buffer.enqueue(3)
    buffer.dequeue()
    buffer.dequeue()
    buffer.enqueue(4)
    buffer.enqueue(5)
    buffer.enqueue(6)

    assertEquals(buffer.dequeue(), 3)
    assertEquals(buffer.dequeue(), 4)
    assertEquals(buffer.dequeue(), 5)
    assertEquals(buffer.dequeue(), 6)
    assert(buffer.isEmpty)
  }

  test("enqueue after full then dequeue returns correct order") {
    val buffer = RingBuffer[String](3)
    buffer.enqueue("a")
    buffer.enqueue("b")
    buffer.enqueue("c")

    assert(buffer.isFull)

    buffer.dequeue()
    assertEquals(buffer.size, 2)
    assert(!buffer.isFull)

    buffer.enqueue("d")

    assertEquals(buffer.dequeue(), "b")
    assertEquals(buffer.dequeue(), "c")
    assertEquals(buffer.dequeue(), "d")
    assert(buffer.isEmpty)
  }

  test("multiple full/empty cycles") {
    val buffer = RingBuffer[Int](2)

    buffer.enqueue(1)
    buffer.enqueue(2)
    assert(buffer.isFull)

    buffer.dequeue()
    buffer.dequeue()
    assert(buffer.isEmpty)

    buffer.enqueue(3)
    buffer.enqueue(4)
    assert(buffer.isFull)

    assertEquals(buffer.dequeue(), 3)
    assertEquals(buffer.dequeue(), 4)
    assert(buffer.isEmpty)
  }

  test("capacity of 1 works correctly") {
    val buffer = RingBuffer[Int](1)

    assert(buffer.isEmpty)
    assert(!buffer.isFull)

    buffer.enqueue(42)
    assert(buffer.isFull)
    assertEquals(buffer.size, 1)
    assertEquals(buffer.peek(), 42)
    assertEquals(buffer.dequeue(), 42)
    assert(buffer.isEmpty)
  }

  test("large capacity works correctly") {
    val buffer = RingBuffer[Int](100)
    (1 to 100).foreach(buffer.enqueue)

    assert(buffer.isFull)
    assertEquals(buffer.size, 100)

    (1 to 100).foreach { expected =>
      assertEquals(buffer.dequeue(), expected)
    }
    assert(buffer.isEmpty)
  }

  test("interleaved enqueue and dequeue") {
    val buffer = RingBuffer[Char](3)

    buffer.enqueue('a')
    assertEquals(buffer.dequeue(), 'a')

    buffer.enqueue('b')
    buffer.enqueue('c')
    assertEquals(buffer.dequeue(), 'b')

    buffer.enqueue('d')
    buffer.enqueue('e')
    assertEquals(buffer.dequeue(), 'c')
    assertEquals(buffer.dequeue(), 'd')
    assertEquals(buffer.dequeue(), 'e')
    assert(buffer.isEmpty)
  }

  test("null elements are supported") {
    val buffer = RingBuffer[String](3)
    buffer.enqueue("first")
    buffer.enqueue(null)
    buffer.enqueue("last")

    assertEquals(buffer.dequeue(), "first")
    assertEquals(buffer.dequeue(), null)
    assertEquals(buffer.dequeue(), "last")
    assert(buffer.isEmpty)
  }

  test("mixed types via generics") {
    val intBuffer = RingBuffer[Int](2)
    val stringBuffer = RingBuffer[String](2)
    val boolBuffer = RingBuffer[Boolean](2)

    intBuffer.enqueue(42)
    stringBuffer.enqueue("test")
    boolBuffer.enqueue(true)

    assertEquals(intBuffer.dequeue(), 42)
    assertEquals(stringBuffer.dequeue(), "test")
    assertEquals(boolBuffer.dequeue(), true)
  }

  test("size is accurate after multiple operations") {
    val buffer = RingBuffer[Int](5)

    assertEquals(buffer.size, 0)
    buffer.enqueue(1)
    assertEquals(buffer.size, 1)
    buffer.enqueue(2)
    assertEquals(buffer.size, 2)
    buffer.enqueue(3)
    assertEquals(buffer.size, 3)

    buffer.dequeue()
    assertEquals(buffer.size, 2)
    buffer.dequeue()
    assertEquals(buffer.size, 1)

    buffer.enqueue(4)
    assertEquals(buffer.size, 2)
    buffer.enqueue(5)
    assertEquals(buffer.size, 3)
    buffer.enqueue(6)
    assertEquals(buffer.size, 4)
    buffer.enqueue(7)
    assertEquals(buffer.size, 5)

    buffer.dequeue()
    assertEquals(buffer.size, 4)
  }
}
