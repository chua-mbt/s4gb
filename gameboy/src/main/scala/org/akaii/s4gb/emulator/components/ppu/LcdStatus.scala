package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/**
 * FF41 — STAT: LCD status
 *
 * @see [[https://gbdev.io/pandocs/STAT.html#ff41--stat-lcd-status]]
 */
case class LcdStatus(
  var lycSelect: Boolean = false,
  var mode2Select: Boolean = false,
  var mode1Select: Boolean = false,
  var mode0Select: Boolean = false,
  var lycEqualsLy: Boolean = false,
  var ppuMode: PpuMode = PpuMode.HorizontalBlank
) {

  /**
   * Initializes this LcdStatus to DMG power-up state.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html#hardware-registers]]
   */
  def initialize(): Unit = {
    lycSelect = true
    mode2Select = false
    mode1Select = false
    mode0Select = false
    lycEqualsLy = true
    ppuMode = PpuMode.VerticalBlank
  }

  def read(lcdEnabled: Boolean): UByte = {
    var b = 0
    if (lycSelect) b |= LcdStatus.Masks.LYC_SELECT.toInt
    if (mode2Select) b |= LcdStatus.Masks.MODE2_SELECT.toInt
    if (mode1Select) b |= LcdStatus.Masks.MODE1_SELECT.toInt
    if (mode0Select) b |= LcdStatus.Masks.MODE0_SELECT.toInt
    if (lycEqualsLy) b |= LcdStatus.Masks.LYC_EQUALS_LY.toInt
    val modeBits = if (lcdEnabled) ppuMode.statValue.toInt & LcdStatus.Masks.PPU_MODE.toInt else 0
    b |= modeBits
    UByte(b)
  }

  def write(byte: UByte): Unit = {
    val b = byte.toInt
    lycSelect = (b & LcdStatus.Masks.LYC_SELECT.toInt) != 0
    mode2Select = (b & LcdStatus.Masks.MODE2_SELECT.toInt) != 0
    mode1Select = (b & LcdStatus.Masks.MODE1_SELECT.toInt) != 0
    mode0Select = (b & LcdStatus.Masks.MODE0_SELECT.toInt) != 0
  }
}

object LcdStatus {
  object Masks {
    val LYC_SELECT: UByte = UByte(0x80)
    val MODE2_SELECT: UByte = UByte(0x40)
    val MODE1_SELECT: UByte = UByte(0x20)
    val MODE0_SELECT: UByte = UByte(0x10)
    val LYC_EQUALS_LY: UByte = UByte(0x04)
    val PPU_MODE: UByte = UByte(0x03)
    val MODE_MASK: UByte = UByte(0xFC)
  }
}
