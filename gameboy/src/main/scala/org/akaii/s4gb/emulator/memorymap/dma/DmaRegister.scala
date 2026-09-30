package org.akaii.s4gb.emulator.memorymap.dma

import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

/**
 * The DMA register ($FF46). Write-only: writing latches the source high byte
 * and marks the transfer active. Reads return garbage.
 *
 * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#ff46--dma-oam-dma-source-address--start]]
 */
class DmaRegister(state: DmaState) extends MemoryMap {

  import DmaRegister.Address.*

  override def apply(address: UShort): UByte =
    if (address == DMA) Ppu.GARBAGE
    else throw new IllegalArgumentException(f"Address not owned by DmaRegister: 0x${address.toInt}%04X")

  override def write(address: UShort, value: UByte): Unit =
    if (address == DMA) {
      state.sourceHighByte = value
      state.isActive = true
    } else {
      throw new IllegalArgumentException(f"Address not owned by DmaRegister: 0x${address.toInt}%04X")
    }
}

object DmaRegister {

  object Address {
    /**
     * FF46 — DMA: OAM DMA source address & start
     *
     * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#ff46--dma-oam-dma-source-address--start]]
     */
    val DMA: UShort = UShort(0xFF46)
  }
}
