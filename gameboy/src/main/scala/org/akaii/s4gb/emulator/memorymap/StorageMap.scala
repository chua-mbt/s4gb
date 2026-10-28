package org.akaii.s4gb.emulator.memorymap

import org.akaii.s4gb.extensions.byteops.*
import spire.math.{UByte, UShort}

import scala.collection.mutable

/**
 * Read/write storage for a contiguous address range.
 *
 * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
 */
final class StorageMap(val start: UShort, val end: UShort) extends RegisterMap {

  require(start.toInt <= end.toInt, f"Invalid storage range: 0x${start.toInt}%04X-0x${end.toInt}%04X")

  override protected val registers: mutable.Map[UShort, UByte] =
    mutable.Map((start.toInt to end.toInt).map(address => (address.toUShort, UByte(0)))*)
}

object StorageMap {

  def apply(start: UShort, end: UShort): StorageMap = new StorageMap(start, end)

  object Address {

    /**
     * A000 — External RAM start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val EXTERNAL_RAM_START: UShort = UShort(0xA000)

    /**
     * BFFF — External RAM end (inclusive).
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val EXTERNAL_RAM_END: UShort = UShort(0xBFFF)

    /**
     * C000 — Work RAM (WRAM) bank 0 start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val WRAM_0_START: UShort = UShort(0xC000)

    /**
     * CFFF — Work RAM (WRAM) bank 0 end (inclusive).
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val WRAM_0_END: UShort = UShort(0xCFFF)

    /**
     * D000 — Work RAM (WRAM) bank 1 start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val WRAM_1_START: UShort = UShort(0xD000)

    /**
     * DFFF — Work RAM (WRAM) bank 1 end (inclusive).
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val WRAM_1_END: UShort = UShort(0xDFFF)

    /**
     * E000 — Echo RAM start, a mirror of C000-DDFF.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html#echo-ram]]
     */
    val ECHO_RAM_START: UShort = UShort(0xE000)

    /**
     * FDFF — Echo RAM end (inclusive).
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html#echo-ram]]
     */
    val ECHO_RAM_END: UShort = UShort(0xFDFF)

    /**
     * FEA0 — Not Usable range start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html#fea0feff-range]]
     */
    val NOT_USABLE_START: UShort = UShort(0xFEA0)

    /**
     * FEFF — Not Usable range end (inclusive).
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html#fea0feff-range]]
     */
    val NOT_USABLE_END: UShort = UShort(0xFEFF)

    /**
     * FF01 — Serial transfer data (SB) start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val SERIAL_START: UShort = UShort(0xFF01)

    /**
     * FF02 — Serial transfer control (SC) end.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val SERIAL_END: UShort = UShort(0xFF02)

    /**
     * FF03 — Unused I/O start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val UNUSED_IO_03_START: UShort = UShort(0xFF03)

    /**
     * FF03 — Unused I/O end.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val UNUSED_IO_03_END: UShort = UShort(0xFF03)

    /**
     * FF08 — Unused I/O start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val UNUSED_IO_08_START: UShort = UShort(0xFF08)

    /**
     * FF0E — Unused I/O end.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val UNUSED_IO_08_END: UShort = UShort(0xFF0E)

    /**
     * FF10 — Audio register start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val AUDIO_START: UShort = UShort(0xFF10)

    /**
     * FF3F — Audio register end.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val AUDIO_END: UShort = UShort(0xFF3F)

    /**
     * FF4C — Unused I/O start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val UNUSED_IO_4C_START: UShort = UShort(0xFF4C)

    /**
     * FF7F — Unused I/O end.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val UNUSED_IO_4C_END: UShort = UShort(0xFF7F)

    /**
     * FF80 — High RAM (HRAM) start.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val HRAM_START: UShort = UShort(0xFF80)

    /**
     * FFFE — High RAM (HRAM) end (inclusive), FFFF is IE.
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val HRAM_END: UShort = UShort(0xFFFE)
  }
}
