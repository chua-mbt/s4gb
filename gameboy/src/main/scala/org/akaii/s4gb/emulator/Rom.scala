package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

/**
 * Cartridge ROM
 *
 * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
 */
case class Rom(
  data: Array[UByte],
  onWrite: Rom.OnWrite = Rom.OnWrite.NoOp
) extends MemoryMap {

  import Rom.Address.*

  override def apply(address: UShort): UByte =
    if (ROM_0_START <= address && address <= ROM_MAX) {
      data(address.toInt)
    } else {
      throw new IllegalArgumentException(f"Invalid ROM address: 0x${address.toInt}%04X")
    }

  override def write(address: UShort, value: UByte): Unit =
    if (ROM_0_START <= address && address <= ROM_MAX) {
      onWrite match {
        case Rom.OnWrite.Throw =>
          throw new IllegalArgumentException(f"Cannot write to ROM address: 0x${address.toInt}%04X")
        case Rom.OnWrite.NoOp =>
          ()
      }
    } else {
      throw new IllegalArgumentException(f"Invalid ROM address: 0x${address.toInt}%04X")
    }

  def load(newData: Array[UByte]): Unit =
    Array.copy(newData, 0, data, 0, data.length)
}

object Rom {
  enum OnWrite {
    case Throw
    case NoOp
  }

  object Address {
    /**
     * ROM start address
     */
    val ROM_START: UShort = ROM_0_START

    /**
     * ROM end address (inclusive)
     */
    val ROM_END: UShort = ROM_1_END

    /**
     * ROM Bank 0 (16 KiB ROM bank 00)
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val ROM_0_START: UShort = UShort(0x0000)
    val ROM_0_END: UShort = UShort(0x3FFF)

    /**
     * ROM Bank 1 (16 KiB ROM Bank 01–NN, switchable)
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val ROM_1_START: UShort = UShort(0x4000)
    val ROM_1_END: UShort = UShort(0x7FFF)

    /**
     * Maximum ROM address
     */
    val ROM_MAX: UShort = ROM_1_END
  }
}
