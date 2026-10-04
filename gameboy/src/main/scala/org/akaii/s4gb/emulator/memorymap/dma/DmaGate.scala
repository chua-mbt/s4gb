package org.akaii.s4gb.emulator.memorymap.dma

import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.memorymap.{Hram, MemoryMap}
import spire.math.{UByte, UShort}

/**
 * Memory map gate that restricts CPU access during a DMA transfer.
 *
 * While a DMA transfer is active, only High RAM ($FF80-$FFFE) is accessible:
 * reads of every other address return garbage and writes are dropped.
 * The transfer component bypasses this gate by holding direct references
 * to its source memory and OAM.
 *
 * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#oam-dma-bus-conflicts]]
 * @see [[https://gbdev.io/pandocs/Memory_Map.html]]
 */
class DmaGate(val state: DmaState, bus: MemoryMap) extends MemoryMap {

  override def apply(address: UShort): UByte =
    if (state.isActive && !isHram(address)) Ppu.GARBAGE else bus(address)

  override def write(address: UShort, value: UByte): Unit =
    if (!state.isActive || isHram(address)) bus.write(address, value)

  private def isHram(address: UShort): Boolean =
    address >= Hram.Address.HRAM_START && address <= Hram.Address.HRAM_END
}
