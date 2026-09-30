package org.akaii.s4gb.emulator.memorymap

import org.akaii.s4gb.extensions.byteops.*
import spire.math.{UByte, UShort}

import scala.collection.mutable

/**
 * High RAM (HRAM), $FF80-$FFFE.
 *
 * 127 bytes of CPU-internal RAM addressed with the 8-bit LDH loads.
 * During OAM DMA on DMG this is the only region the CPU may access.
 *
 * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
 * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#oam-dma-bus-conflicts]]
 */
class Hram extends RegisterMap {

  import Hram.Address.*

  override protected val registers: mutable.Map[UShort, UByte] =
    mutable.Map((HRAM_START.toInt to HRAM_END.toInt).map(address => (address.toUShort, UByte(0)))*)
}

object Hram {

  def apply(): Hram = new Hram

  object Address {
    /**
     * FF80 — High RAM start
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val HRAM_START: UShort = UShort(0xFF80)

    /**
     * FFFE — High RAM end (inclusive), FFFF is IE
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
     */
    val HRAM_END: UShort = UShort(0xFFFE)
  }
}
