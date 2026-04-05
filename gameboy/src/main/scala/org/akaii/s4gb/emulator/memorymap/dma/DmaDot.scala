package org.akaii.s4gb.emulator.memorymap.dma

/**
 * A "dot" representing one cycle of OAM DMA transfer.
 *
 * DMA transfers 160 bytes (OAM size) from a source address to OAM, taking exactly 160 dots to complete.
 *
 * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#ff46--dma-oam-dma-source-address--start]]
 */
class DmaDot private (var current: Int, var cumulative: Int) {
  def isBoundary: Boolean = current == DmaDot.DOTS_PER_TRANSFER
}

object DmaDot {
  def apply(): DmaDot = new DmaDot(0, 0)

  def apply(current: Int, cumulative: Int): DmaDot = new DmaDot(current, cumulative)

  /**
   * Number of dots (t-cycles) required to complete OAM DMA transfer.
   * Transfer is 160 bytes × 4 t-cycles per byte = 640 t-cycles.
   *
   * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html]]
   */
  val DOTS_PER_TRANSFER: Int = 640
}
