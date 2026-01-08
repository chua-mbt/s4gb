package org.akaii.s4gb.emulator

/**
 * Represents an abstraction of the GameBoy's 16-bit address bus
 *
 * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
 */
trait MemoryMap() {
  def apply(address: Int): Int

  def write(address: Int, value: Int): Unit
}

object MemoryMap {
  val MEMORY_SIZE: Int = 0x10000 // 64KB
}
