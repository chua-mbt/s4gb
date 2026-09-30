package org.akaii.s4gb.emulator.memorymap.dma

import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import org.akaii.s4gb.extensions.byteops.*

/**
 * OAM DMA Transfer
 *
 * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html]]
 */
object DmaTransfer {
  /**
   * Number of dots (t-cycles) per byte transfer.
   * Transfer is 160 bytes × 4 t-cycles per byte = 640 t-cycles.
   *
   * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#ff46--dma-oam-dma-source-address--start]]
   */
  val DOTS_PER_BYTE: Int = 4

  /**
   * Number of dots (t-cycles) required to complete OAM DMA transfer.
   *
   * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html]]
   */
  val DOTS_PER_TRANSFER: Int = 640

  def tick(state: DmaState, memory: MemoryMap, oam: MemoryMap): Unit = {
    if (!state.isActive) return

    if (state.dot % DOTS_PER_BYTE == 0) {
      val byteIndex = state.dot / DOTS_PER_BYTE
      if (byteIndex < Ppu.OAM_SIZE) {
        val sourceAddress = ((state.sourceHighByte.toInt << 8) | byteIndex).toUShort
        val oamAddress = (Ppu.Address.OAM.START.toInt + byteIndex).toUShort
        oam.write(oamAddress, memory(sourceAddress))
      }
    }

    state.dot += 1

    if (state.dot >= DOTS_PER_TRANSFER) {
      state.isActive = false
      state.dot = 0
    }
  }
}
