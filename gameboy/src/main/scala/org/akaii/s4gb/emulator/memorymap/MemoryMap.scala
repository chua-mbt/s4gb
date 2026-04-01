package org.akaii.s4gb.emulator.memorymap

import spire.math.{UByte, UShort}

import scala.util.Try

/**
 * Represents an abstraction of the GameBoy's 16-bit address bus
 *
 * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
 */
trait MemoryMap() {
  def apply(address: UShort): UByte
  def write(address: UShort, value: UByte): Unit

  def fetchIfPresent(address: UShort): Option[UByte] = Try(apply(address)).toOption
}

object MemoryMap {
  val MEMORY_SIZE: Int = 0x10000 // 64KB

  /**
   * IO Registers
   *
   * @see [[https://gbdev.io/pandocs/Memory_Map.html#io-ranges]]
   * @see [[https://gbdev.io/pandocs/Hardware_Reg_List.html]]
   **/
  val IO_REGISTERS_START: UShort = UShort(0xFF00)
}
