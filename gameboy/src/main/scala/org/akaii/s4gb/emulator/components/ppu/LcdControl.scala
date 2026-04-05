package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.extensions.byteops.toUByte
import spire.math.UByte

/**
 * FF40 — LCDC: LCD Control
 *
 * @see https://gbdev.io/pandocs/LCDC.html
 */
case class LcdControl(
  var lcdEnable: Boolean = false,
  var windowTileMap: Boolean = false,
  var windowEnable: Boolean = false,
  var bgWindowTileData: Boolean = false,
  var bgTileMap: Boolean = false,
  var objSize: Boolean = false,
  var objEnable: Boolean = false,
  var bgEnable: Boolean = false
) {

  /**
   * Initializes this LcdStatus to DMG power-up state.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html#hardware-registers]]
   */
  def initialize(): Unit = {
    lcdEnable = true
    windowTileMap = false
    windowEnable = false
    bgWindowTileData = true
    bgTileMap = false
    objSize = false
    objEnable = false
    bgEnable = true
  }

  def read(): UByte = {
    var b = 0

    if (bgEnable) b |= LcdControl.Masks.BG_ENABLE.toInt
    if (objEnable) b |= LcdControl.Masks.OBJ_ENABLE.toInt
    if (objSize) b |= LcdControl.Masks.OBJ_SIZE.toInt
    if (bgTileMap) b |= LcdControl.Masks.BG_TILE_MAP.toInt
    if (bgWindowTileData) b |= LcdControl.Masks.BG_WINDOW_TILE_DATA.toInt
    if (windowEnable) b |= LcdControl.Masks.WINDOW_ENABLE.toInt
    if (windowTileMap) b |= LcdControl.Masks.WINDOW_TILE_MAP.toInt
    if (lcdEnable) b |= LcdControl.Masks.LCD_ENABLE.toInt

    UByte(b)
  }

  def write(byte: UByte): Unit = {
    val b = byte.toInt

    lcdEnable = (b & LcdControl.Masks.LCD_ENABLE.toInt) != 0
    windowTileMap = (b & LcdControl.Masks.WINDOW_TILE_MAP.toInt) != 0
    windowEnable = (b & LcdControl.Masks.WINDOW_ENABLE.toInt) != 0
    bgWindowTileData = (b & LcdControl.Masks.BG_WINDOW_TILE_DATA.toInt) != 0
    bgTileMap = (b & LcdControl.Masks.BG_TILE_MAP.toInt) != 0
    objSize = (b & LcdControl.Masks.OBJ_SIZE.toInt) != 0
    objEnable = (b & LcdControl.Masks.OBJ_ENABLE.toInt) != 0
    bgEnable = (b & LcdControl.Masks.BG_ENABLE.toInt) != 0
  }
}

object LcdControl {

  object Masks {
    val LCD_ENABLE: UByte = UByte(0x80)
    val WINDOW_TILE_MAP: UByte = UByte(0x40)
    val WINDOW_ENABLE: UByte = UByte(0x20)
    val BG_WINDOW_TILE_DATA: UByte = UByte(0x10)
    val BG_TILE_MAP: UByte = UByte(0x08)
    val OBJ_SIZE: UByte = UByte(0x04)
    val OBJ_ENABLE: UByte = UByte(0x02)
    val BG_ENABLE: UByte = UByte(0x01)
  }

  private val SPRITE_SIZE_BIT: UByte = 0x04.toUByte
  private val SPRITE_HEIGHT_8PX: Int = 8
  private val SPRITE_HEIGHT_16PX: Int = 16

  def spriteHeight(lcdc: UByte): Int =
    if ((lcdc & SPRITE_SIZE_BIT) == 0.toUByte) SPRITE_HEIGHT_8PX else SPRITE_HEIGHT_16PX
}
