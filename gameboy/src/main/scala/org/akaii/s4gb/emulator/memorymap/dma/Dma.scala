package org.akaii.s4gb.emulator.memorymap.dma

import spire.math.UShort

/**
 * OAM DMA Transfer
 *
 * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html]]
 */
object Dma {
  //def transfer(state: Ppu.State): Unit = ??? // TODO implement transfer

  object Address {
    /**
     * DMA: OAM DMA source address & start
     *
     * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#ff46--dma-oam-dma-source-address--start]]
     */
    val DMA: UShort = UShort(0xFF46)
  }
}
