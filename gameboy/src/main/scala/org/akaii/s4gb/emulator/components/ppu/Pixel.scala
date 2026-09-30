package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.collections.Preallocated
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Gameboy Pixel
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html?highlight=fifo#pixel-fifo]]
 * @see [[https://gbdev.io/pandocs/Tile_Data.html#data-format]]
 */
sealed trait Pixel

object Pixel {
  private val colorIndexMask: UByte = UByte(0x03)
  private val bitsPerColorIndex: Int = 2

  /**
   * Resolve color for palette
   * @see [[https://gbdev.io/pandocs/Palettes.html#ff47--bgp-non-cgb-mode-only-bg-palette-data]]
   */
  def resolvePixelColor(colorIndex: UByte, palette: UByte): UByte = {
    val shift = colorIndex.toInt * bitsPerColorIndex
    ((palette.toInt >> shift) & colorIndexMask.toInt).toUByte
  }

}

case class BackgroundPixel(var colorIndex: UByte) extends Pixel

object BackgroundPixel {

  val lightestColor: UByte = UByte(0)

  def empty: BackgroundPixel = BackgroundPixel(lightestColor)

  given Preallocated[BackgroundPixel] with {
    def allocate: BackgroundPixel = BackgroundPixel.empty

    def copyInto(target: BackgroundPixel, source: BackgroundPixel): Unit =
      target.colorIndex = source.colorIndex
  }
}

case class ObjectPixel(
  var colorIndex: UByte,
  var usePalette0: Boolean = true,
  var backgroundPriority: Boolean = false
) extends Pixel {
  def isOpaque: Boolean = colorIndex != ObjectPixel.transparentColor
  def isOverBackground: Boolean = !backgroundPriority
}

object ObjectPixel {

  val transparentColor: UByte = UByte(0)

  def empty: ObjectPixel = ObjectPixel(transparentColor)

  given Preallocated[ObjectPixel] with {
    def allocate: ObjectPixel = ObjectPixel.empty

    def copyInto(target: ObjectPixel, source: ObjectPixel): Unit = {
      target.colorIndex = source.colorIndex
      target.usePalette0 = source.usePalette0
      target.backgroundPriority = source.backgroundPriority
    }
  }
}
