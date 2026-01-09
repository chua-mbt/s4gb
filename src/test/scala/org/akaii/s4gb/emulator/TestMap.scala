package org.akaii.s4gb.emulator

import spire.math.{UByte, UShort}

case class TestMap() extends MemoryMap {
  private val memory: Array[UByte] = Array.fill(MemoryMap.MEMORY_SIZE)(UByte.MinValue)

  override def apply(address: UShort): UByte =
    memory(address.toInt)

  override def read(address: UShort, length: Int): Array[UByte] =
    memory.slice(address.toInt, address.toInt + length)

  override def write(address: UShort, value: UByte): Unit =
    memory.update(address.toInt, value)
}
