package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Gameboy terminology for sprites. Max of 40 can be displayed, 10 max on any scanline.
 *
 * @see [[https://gbdev.io/pandocs/Graphics.html#objects]]
 */
case class GameboyObject(
  var y: UByte = 0.toUByte,
  var x: UByte = 0.toUByte,
  var tileIndex: UByte = 0.toUByte,
  var attributes: UByte = 0.toUByte,
  var used: Boolean = false
) {
  @inline def notInUse: Boolean = !used

  /**
   * OAM byte 3 bit 7 — OBJ-to-BG priority. 1 means BG/Window colors 1-3 are drawn over this object.
   */
  @inline def backgroundPriority: Boolean = attribute(GameboyObject.BG_PRIORITY_BIT)

  /**
   * OAM byte 3 bit 6 — vertical mirror of the whole object.
   */
  @inline def yFlipped: Boolean = attribute(GameboyObject.Y_FLIP_BIT)

  /**
   * OAM byte 3 bit 5 — horizontal mirror of the whole object.
   */
  @inline def xFlipped: Boolean = attribute(GameboyObject.X_FLIP_BIT)

  /**
   * OAM byte 3 bit 4 — DMG palette select. 0 selects OBP0, 1 selects OBP1.
   */
  @inline def usePalette0: Boolean = !attribute(GameboyObject.PALETTE_BIT)

  /**
   * Convert a screen row into the object's own coordinate space, where row 0 is
   * the object's top edge rather than the top of the screen, then apply Y flip.
   *
   * Example: an object at Y=30 starts on screen row 14, so screen row 20 is row
   * 6 of the object. With Y flip the object is read upside down instead.
   *
   * @see [[https://gbdev.io/pandocs/OAM.html#byte-3--attributesflags]]
   */
  @inline def lineForScreenRow(ly: Int, height: Int): Int = {
    val lineInObject = ly - GameboyObject.topScreenRow(y.toInt)
    // Rows are 0..height-1, so the flip swaps 0 with height-1.
    if (yFlipped) height - 1 - lineInObject else lineInObject
  }

  /**
   * 8x16 objects ignore the low bit of the OAM tile index and pick the top or
   * bottom tile from the resolved row.
   *
   * @see [[https://gbdev.io/pandocs/OAM.html#byte-2--tile-index]]
   */
  @inline def tileNumberFor(lineInObject: Int, tall: Boolean): Int =
    if (tall) (tileIndex.toInt & ~1) | ((lineInObject / Tile.SIZE) & 1) else tileIndex.toInt

  @inline def reset(): Unit = used = false

  @inline def set(y: UByte, x: UByte, tileIndex: UByte, attributes: UByte): Unit = {
    this.y = y
    this.x = x
    this.tileIndex = tileIndex
    this.attributes = attributes
    used = true
  }

  @inline private def attribute(bit: Int): Boolean = ((attributes.toInt >> bit) & 1) != 0
}

object GameboyObject {

  /** Bytes per object in OAM. */
  private[ppu] val BYTE_SIZE: Int = 4

  /** OAM byte 0 is biased by 16, so an object's top edge sits at `y - 16`. */
  private[ppu] val Y_OFFSET: Int = 16

  /** Screen row of an object's top edge, given its raw OAM byte 0. */
  @inline private[ppu] def topScreenRow(y: Int): Int = y - Y_OFFSET

  /**
   * OAM byte 3 bit layout.
   *
   * @see [[https://gbdev.io/pandocs/OAM.html#byte-3--attributesflags]]
   */
  private val BG_PRIORITY_BIT: Int = 7
  private val Y_FLIP_BIT: Int = 6
  private val X_FLIP_BIT: Int = 5
  private val PALETTE_BIT: Int = 4

  @inline def empty: GameboyObject = GameboyObject()
}
