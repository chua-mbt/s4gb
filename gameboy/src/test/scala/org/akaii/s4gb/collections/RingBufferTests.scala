package org.akaii.s4gb.collections

import munit.FunSuite

class RingBufferTests extends FunSuite {

  test("isEmpty on creation") {
    val buffer = RingBuffer[Item](3)
    assert(buffer.isEmpty)
    assert(!buffer.isFull)
    assertEquals(buffer.size, 0)
  }

  test("enqueue and dequeue - single element") {
    val buffer = RingBuffer[Item](3)
    buffer.enqueue(Item(text = "a"))

    assert(!buffer.isEmpty)
    assertEquals(buffer.size, 1)

    val out = destination(1)
    assertEquals(buffer.dequeue(out), 1)
    assertEquals(out(0).text, "a")
    assert(buffer.isEmpty)
    assertEquals(buffer.size, 0)
  }

  test("enqueue and dequeue - multiple elements in order") {
    val buffer = RingBuffer[Item](5)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    buffer.enqueue(Item(number = 3))

    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.number).toList, List(1, 2, 3))
    assert(buffer.isEmpty)
  }

  test("clear empties the buffer") {
    val buffer = RingBuffer[Item](4)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    buffer.clear()

    assert(buffer.isEmpty)
    assertEquals(buffer.size, 0)
  }

  test("clear allows reuse after wrap around") {
    val buffer = RingBuffer[Item](4)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    assertEquals(buffer.dequeue(destination(1)), 1)
    buffer.enqueue(Item(number = 3))

    buffer.clear()

    buffer.enqueue(Item(number = 4))
    buffer.enqueue(Item(number = 5))

    val out = destination(2)
    assertEquals(buffer.dequeue(out), 2)
    assertEquals(out.map(_.number).toList, List(4, 5))
  }

  test("dequeue on empty returns 0") {
    val buffer = RingBuffer[Item](3)
    assertEquals(buffer.dequeue(destination(3)), 0)
  }

  test("dequeue after clear returns 0") {
    val buffer = RingBuffer[Item](2)
    buffer.enqueue(Item(number = 1))
    buffer.clear()

    assertEquals(buffer.dequeue(destination(2)), 0)
  }

  test("dequeue stops at available items when destination is larger") {
    val buffer = RingBuffer[Item](5)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))

    val out = destination(4)
    assertEquals(buffer.dequeue(out), 2)
    assertEquals(out.map(_.number).toList, List(1, 2, 0, 0))
    assert(buffer.isEmpty)
    assertEquals(buffer.size, 0)
  }

  test("dequeue clamps to destination size") {
    val buffer = RingBuffer[Item](5)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    buffer.enqueue(Item(number = 3))

    val out = destination(2)
    assertEquals(buffer.dequeue(out), 2)
    assertEquals(out.map(_.number).toList, List(1, 2))
    assertEquals(buffer.size, 1)
  }

  test("dequeue copies into caller owned instances") {
    val buffer = RingBuffer[Item](3)
    buffer.enqueue(Item(number = 7))

    val reusable = Item(number = -1)
    val out = Array(reusable)
    assertEquals(buffer.dequeue(out), 1)
    assert(out(0) eq reusable)
    assertEquals(reusable.number, 7)
  }

  test("isFull when at capacity") {
    val buffer = RingBuffer[Item](2)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))

    assert(buffer.isFull)
    assertEquals(buffer.size, 2)
  }

  test("enqueue on full does not overwrite") {
    val buffer = RingBuffer[Item](2)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    buffer.enqueue(Item(number = 3))

    val out = destination(2)
    assertEquals(buffer.dequeue(out), 2)
    assertEquals(out.map(_.number).toList, List(1, 2))
    assert(buffer.isEmpty)
  }

  test("wrap around - head and tail wrap") {
    val buffer = RingBuffer[Item](4)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    buffer.enqueue(Item(number = 3))
    assertEquals(buffer.dequeue(destination(2)), 2)
    buffer.enqueue(Item(number = 4))
    buffer.enqueue(Item(number = 5))
    buffer.enqueue(Item(number = 6))

    val out = destination(4)
    assertEquals(buffer.dequeue(out), 4)
    assertEquals(out.map(_.number).toList, List(3, 4, 5, 6))
    assert(buffer.isEmpty)
  }

  test("enqueue after full then dequeue returns correct order") {
    val buffer = RingBuffer[Item](3)
    buffer.enqueue(Item(text = "a"))
    buffer.enqueue(Item(text = "b"))
    buffer.enqueue(Item(text = "c"))

    assert(buffer.isFull)

    assertEquals(buffer.dequeue(destination(1)), 1)
    assertEquals(buffer.size, 2)
    assert(!buffer.isFull)

    buffer.enqueue(Item(text = "d"))

    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.text).toList, List("b", "c", "d"))
    assert(buffer.isEmpty)
  }

  test("multiple full/empty cycles") {
    val buffer = RingBuffer[Item](2)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    assert(buffer.isFull)

    val first = destination(2)
    assertEquals(buffer.dequeue(first), 2)
    assertEquals(first.map(_.number).toList, List(1, 2))
    assert(buffer.isEmpty)

    buffer.enqueue(Item(number = 3))
    buffer.enqueue(Item(number = 4))
    assert(buffer.isFull)

    val second = destination(2)
    assertEquals(buffer.dequeue(second), 2)
    assertEquals(second.map(_.number).toList, List(3, 4))
    assert(buffer.isEmpty)
  }

  test("capacity of 1 works correctly") {
    val buffer = RingBuffer[Item](1)
    assert(buffer.isEmpty)
    assert(!buffer.isFull)

    buffer.enqueue(Item(number = 42))
    assert(buffer.isFull)
    assertEquals(buffer.size, 1)

    val out = destination(1)
    assertEquals(buffer.dequeue(out), 1)
    assertEquals(out(0).number, 42)
    assert(buffer.isEmpty)
  }

  test("large capacity works correctly") {
    val buffer = RingBuffer[Item](100)
    (1 to 100).foreach(i => buffer.enqueue(Item(number = i)))

    assert(buffer.isFull)
    assertEquals(buffer.size, 100)

    val out = destination(100)
    assertEquals(buffer.dequeue(out), 100)
    assertEquals(out.map(_.number).toList, (1 to 100).toList)
    assert(buffer.isEmpty)
  }

  test("interleaved enqueue and dequeue") {
    val buffer = RingBuffer[Item](3)

    buffer.enqueue(Item(letter = 'a'))
    assertEquals(buffer.dequeue(destination(1)), 1)

    buffer.enqueue(Item(letter = 'b'))
    buffer.enqueue(Item(letter = 'c'))
    val middle = destination(1)
    assertEquals(buffer.dequeue(middle), 1)
    assertEquals(middle(0).letter, 'b')

    buffer.enqueue(Item(letter = 'd'))
    buffer.enqueue(Item(letter = 'e'))
    val rest = destination(3)
    assertEquals(buffer.dequeue(rest), 3)
    assertEquals(rest.map(_.letter).toList, List('c', 'd', 'e'))
    assert(buffer.isEmpty)
  }

  test("mixed fields round trip") {
    val buffer = RingBuffer[Item](2)
    buffer.enqueue(Item(number = 7, text = "seven", flag = true, letter = 'v'))
    buffer.enqueue(Item(number = 8, text = "eight", flag = false, letter = 'g'))

    val out = destination(2)
    assertEquals(buffer.dequeue(out), 2)

    val first = out(0)
    assertEquals(first.number, 7)
    assertEquals(first.text, "seven")
    assert(first.flag)
    assertEquals(first.letter, 'v')

    val second = out(1)
    assertEquals(second.number, 8)
    assertEquals(second.text, "eight")
    assert(!second.flag)
    assertEquals(second.letter, 'g')
  }

  test("size is accurate after multiple operations") {
    val buffer = RingBuffer[Item](5)
    assertEquals(buffer.size, 0)
    buffer.enqueue(Item(number = 1))
    assertEquals(buffer.size, 1)
    buffer.enqueue(Item(number = 2))
    assertEquals(buffer.size, 2)
    buffer.enqueue(Item(number = 3))
    assertEquals(buffer.size, 3)

    assertEquals(buffer.dequeue(destination(1)), 1)
    assertEquals(buffer.size, 2)
    assertEquals(buffer.dequeue(destination(1)), 1)
    assertEquals(buffer.size, 1)

    buffer.enqueue(Item(number = 4))
    assertEquals(buffer.size, 2)
    buffer.enqueue(Item(number = 5))
    assertEquals(buffer.size, 3)
    buffer.enqueue(Item(number = 6))
    assertEquals(buffer.size, 4)
    buffer.enqueue(Item(number = 7))
    assertEquals(buffer.size, 5)

    assertEquals(buffer.dequeue(destination(1)), 1)
    assertEquals(buffer.size, 4)
  }

  test("enqueueAll adds multiple elements") {
    val buffer = RingBuffer[Item](5)
    buffer.enqueueAll(Seq(Item(number = 1), Item(number = 2), Item(number = 3)))

    assertEquals(buffer.size, 3)
    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.number).toList, List(1, 2, 3))
    assert(buffer.isEmpty)
  }

  test("enqueueAll respects capacity") {
    val buffer = RingBuffer[Item](3)
    buffer.enqueueAll(Seq(Item(number = 1), Item(number = 2), Item(number = 3), Item(number = 4)))

    assertEquals(buffer.size, 3)
    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.number).toList, List(1, 2, 3))
    assert(buffer.isEmpty)
  }

  test("enqueue copies value - mutations to original do not affect buffer") {
    val buffer = RingBuffer[Item](3)
    val original = Item(number = 42, text = "before", flag = false, letter = 'a')
    buffer.enqueue(original)

    original.number = 99
    original.text = "after"
    original.flag = true
    original.letter = 'z'

    val out = destination(1)
    assertEquals(buffer.dequeue(out), 1)
    val stored = out(0)
    assertEquals(stored.number, 42)
    assertEquals(stored.text, "before")
    assert(!stored.flag)
    assertEquals(stored.letter, 'a')
  }

  test("pop removes and returns item when not empty") {
    val buffer = RingBuffer[Item](3)
    val item1 = Item(number = 1)
    val item2 = Item(number = 2)
    buffer.enqueue(item1)
    buffer.enqueue(item2)

    val out = Item()
    assert(buffer.pop(out))
    assertEquals(out.number, 1)
    assertEquals(buffer.size, 1)

    assert(buffer.pop(out))
    assertEquals(out.number, 2)
    assertEquals(buffer.size, 0)
  }

  test("pop on empty returns false") {
    val buffer = RingBuffer[Item](3)
    val out = Item()
    assert(!buffer.pop(out))
    assertEquals(buffer.size, 0)
  }

  test("fillFromHead fills an empty buffer to capacity") {
    val buffer = RingBuffer[Item](3)
    var anyOccupied = false
    buffer.fillFromHead { (_, occupied, offset) =>
      if (occupied) anyOccupied = true
      Item(number = offset + 1)
    }

    assert(!anyOccupied)
    assertEquals(buffer.size, 3)
    assert(buffer.isFull)

    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.number).toList, List(1, 2, 3))
  }

  test("fillFromHead offsets start at the head, not the backing index") {
    val buffer = RingBuffer[Item](3)
    buffer.enqueue(Item(number = 10))
    buffer.enqueue(Item(number = 20))
    buffer.enqueue(Item(number = 30))
    assertEquals(buffer.dequeue(destination(1)), 1)

    buffer.fillFromHead((_, _, offset) => Item(number = offset))

    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.number).toList, List(0, 1, 2))
  }

  test("fillFromHead keeps a live cell when the caller returns the current value") {
    val buffer = RingBuffer[Item](3)
    buffer.enqueue(Item(number = 7))

    buffer.fillFromHead { (current, occupied, offset) =>
      if (occupied) current else Item(number = 100 + offset)
    }

    assertEquals(buffer.size, 3)
    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.number).toList, List(7, 101, 102))
  }

  test("fillFromHead replaces a live cell when the caller returns a new value") {
    val buffer = RingBuffer[Item](3)
    buffer.enqueue(Item(number = 7))

    buffer.fillFromHead((_, _, offset) => Item(number = offset))

    val out = destination(3)
    assertEquals(buffer.dequeue(out), 3)
    assertEquals(out.map(_.number).toList, List(0, 1, 2))
  }

  test("fillFromHead offers a drained cell as unoccupied despite its stale value") {
    val buffer = RingBuffer[Item](2)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    assertEquals(buffer.dequeue(destination(2)), 2)

    var seen = List.empty[(Int, Boolean)]
    buffer.fillFromHead { (current, occupied, offset) =>
      seen = seen :+ ((current.number, occupied))
      Item(number = 50 + offset)
    }

    assertEquals(seen, List((1, false), (2, false)))
    assertEquals(buffer.size, 2)
  }

  test("fillFromHead forces size to capacity from a partial buffer") {
    val buffer = RingBuffer[Item](4)
    buffer.enqueue(Item(number = 1))
    buffer.enqueue(Item(number = 2))
    assertEquals(buffer.size, 2)

    buffer.fillFromHead((_, _, _) => Item(number = 9))

    assertEquals(buffer.size, 4)
    assert(buffer.isFull)
  }

  test("fillFromHead copies the caller's value rather than storing it") {
    val buffer = RingBuffer[Item](2)
    val supplied = Item(number = 5)
    buffer.fillFromHead((_, _, _) => supplied)
    supplied.number = 99

    val out = destination(2)
    assertEquals(buffer.dequeue(out), 2)
    assertEquals(out.map(_.number).toList, List(5, 5))
  }
}

case class Item(
  var number: Int = 0,
  var text: String = "",
  var flag: Boolean = false,
  var letter: Char = ' '
)

given Preallocated[Item] with {
  def allocate: Item = Item()
  def copyInto(target: Item, source: Item): Unit = {
    target.number = source.number
    target.text = source.text
    target.flag = source.flag
    target.letter = source.letter
  }
}

def destination(size: Int): Array[Item] = Array.fill(size)(Item())
