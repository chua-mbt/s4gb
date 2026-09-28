package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.collections.Preallocated
import spire.math.UByte

/**
 * Gameboy Pixel
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html?highlight=fifo#pixel-fifo]]
 */
class Pixel(
  var colorIndex: UByte,
  var palette: UByte = Pixel.BG_PALETTE,
  var spritePriority: Int = Pixel.BG_SPRITE_PRIORITY,
  var bgPriority: Boolean = false
)

object Pixel {
  // palette and sprite priority are N/A for background pixels
  val BG_PALETTE: UByte = UByte(0)
  val BG_SPRITE_PRIORITY: Int = 0

  given Preallocated[Pixel] with {
    def allocate: Pixel = Pixel(Pixel.BG_PALETTE)
    def copyInto(target: Pixel, source: Pixel): Unit = {
      target.colorIndex = source.colorIndex
      target.palette = source.palette
      target.spritePriority = source.spritePriority
      target.bgPriority = source.bgPriority
    }
  }
}
