package org.akaii.s4gb.emulator

import spire.math.{UByte, UShort}

case class TestMap() extends MemoryMap {
  val underlying: Array[UByte] = Array.fill(MemoryMap.MEMORY_SIZE)(UByte.MinValue)

  override def apply(address: UShort): UByte =
    underlying(address.toInt)

  override def read(address: UShort, length: Int): Array[UByte] =
    underlying.slice(address.toInt, address.toInt + length)

  override def write(address: UShort, value: UByte): Unit =
    underlying.update(address.toInt, value)

  override def equals(obj: Any): Boolean = obj match {
    case that: TestMap => this.underlying.sameElements(that.underlying)
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(underlying.map(_.toInt))
}
