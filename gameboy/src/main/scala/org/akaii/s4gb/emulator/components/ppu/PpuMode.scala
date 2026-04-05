package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.extensions.byteops.toUByte
import spire.math.UByte

/**
 * PPU Modes
 *
 * @see [[https://gbdev.io/pandocs/Rendering.html#highlight=mode#ppu-modes]]
 * @see [[https://gbdev.io/pandocs/Accessing_VRAM_and_OAM.html]]
 */
sealed abstract class PpuMode(val statValue: UByte, val canAccessVram: Boolean, val canAccessOam: Boolean) {
  def tick(state: Ppu.State): PpuMode
}

object PpuMode {
  case object HorizontalBlank extends PpuMode(UByte(0x00), canAccessVram = true, canAccessOam = true) {
    private val END_LY: UByte = 143.toUByte

    override def tick(state: Ppu.State): PpuMode =
      state.ly match {
        case END_LY => VerticalBlank
        case _ if state.scanlineDot.isBoundary => OamScan
        case _ => HorizontalBlank
      }
  }

  case object VerticalBlank extends PpuMode(UByte(0x01), canAccessVram = true, canAccessOam = true) {
    private val END_LY: UByte = Ppu.SCANLINES_PER_FRAME - 1.toUByte

    override def tick(state: Ppu.State): PpuMode =
      if (state.ly == END_LY) OamScan else VerticalBlank
  }

  case object OamScan extends PpuMode(UByte(0x02), canAccessVram = true, canAccessOam = false) {
    val END_DOT: Int = 79

    override def tick(state: Ppu.State): PpuMode = {
      OamScanner.scan(state)
      if (state.scanlineDot.current >= END_DOT) Draw else OamScan
    }
  }

  case object Draw extends PpuMode(UByte(0x03), canAccessVram = false, canAccessOam = false) {
    override def tick(state: Ppu.State): PpuMode = this // TODO
  }
}
