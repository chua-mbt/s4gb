package org.akaii.s4gb.emulator.components.ppu

/**
 * A "dot" = one 222 Hz (≅ 4.194 MHz) time unit. Dots remain the same regardless of whether the
 * CPU is in Double Speed mode, so there are 4 dots per Normal Speed M-cycle, and 2 per Double Speed M-cycle.
 *
 * @see [[https://gbdev.io/pandocs/Rendering.html#terminology]]
 */
class ScanlineDot private (var current: Int, var cumulative: Int) {
  def isBoundary: Boolean = current == ScanlineDot.DOTS_PER_LINE
}

object ScanlineDot {
  def apply(): ScanlineDot = new ScanlineDot(0, 0)

  def apply(current: Int, cumulative: Int): ScanlineDot = new ScanlineDot(current, cumulative)

  /**
   * Number of dots in scanline (ly).
   *
   * @see [[https://gbdev.io/pandocs/Rendering.html]]
   */
  val DOTS_PER_LINE: Int = 456
}
