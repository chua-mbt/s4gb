package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/**
 * Gameboy Pixel
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html?highlight=fifo#pixel-fifo]]
 */
case class Pixel(
  colorIndex: UByte,
  palette: UByte,
  spritePriority: Int,
  bgPriority: Boolean
)

object Pixel {
  val TRANSPARENT: Pixel = Pixel(UByte(0), UByte(0), 0, false)
}
