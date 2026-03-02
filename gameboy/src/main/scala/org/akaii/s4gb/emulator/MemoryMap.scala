package org.akaii.s4gb.emulator

import spire.math.{UByte, UShort}

/**
 * Represents an abstraction of the GameBoy's 16-bit address bus
 *
 * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
 */
trait MemoryMap() {
  def apply(address: UShort): UByte
  def read(address: UShort, length: Int): Array[UByte]
  def write(address: UShort, value: UByte): Unit
}

object MemoryMap {
  val MEMORY_SIZE: Int = 0x10000 // 64KB

  /**
   * IO Registers
   * @see [[https://gbdev.io/pandocs/Memory_Map.html#io-ranges]]
   **/
  val IO_REGISTERS_START: UShort = UShort(0xFF00)
}
