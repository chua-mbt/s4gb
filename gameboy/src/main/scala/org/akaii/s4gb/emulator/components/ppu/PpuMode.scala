package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * PPU Modes
 *
 * @see [[https://gbdev.io/pandocs/Rendering.html#highlight=mode#ppu-modes]]
 * @see [[https://gbdev.io/pandocs/Accessing_VRAM_and_OAM.html]]
 */
sealed abstract class PpuMode(val statValue: UByte, val canAccessVram: Boolean, val canAccessOam: Boolean) {
  def tick(state: Ppu.State, interrupts: Interrupts): PpuMode

  protected def interruptsAndTransition(target: PpuMode, state: Ppu.State, interrupts: Interrupts): PpuMode = {
    val vBlankEntry = target == PpuMode.VerticalBlank
    val hblankLStatInterrupt = target == PpuMode.HorizontalBlank && state.lcdStatus.mode0Select
    val vblankLStatInterrupt = vBlankEntry && state.lcdStatus.mode1Select
    val oamScanLStatInterrupt = target == PpuMode.OamScan && state.lcdStatus.mode2Select
    val lStatInterrupt = hblankLStatInterrupt || vblankLStatInterrupt || oamScanLStatInterrupt

    if (vBlankEntry) interrupts.request(Interrupts.Source.VBlank)
    if (lStatInterrupt) interrupts.request(Interrupts.Source.LCDStat)
    target
  }
}

object PpuMode {
  val VISIBLE_SCANLINES_END: UByte = 143.toUByte
  val VBLANK_START_LY: UByte = 144.toUByte
  val FRAME_WRAP_LY: UByte = 0.toUByte

  case object HorizontalBlank extends PpuMode(UByte(0x00), canAccessVram = true, canAccessOam = true) {
    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode =
      state.ly match {
        case VBLANK_START_LY if state.scanlineDot.isBoundary =>
          interruptsAndTransition(VerticalBlank, state, interrupts)
        case _ if state.scanlineDot.isBoundary =>
          interruptsAndTransition(OamScan, state, interrupts)
        case _ =>
          HorizontalBlank
      }
  }

  case object VerticalBlank extends PpuMode(UByte(0x01), canAccessVram = true, canAccessOam = true) {
    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode =
      state.ly match {
        case FRAME_WRAP_LY if state.scanlineDot.isBoundary =>
          interruptsAndTransition(OamScan, state, interrupts)
        case _ =>
          VerticalBlank
      }
  }

  case object OamScan extends PpuMode(UByte(0x02), canAccessVram = true, canAccessOam = false) {
    val END_DOT: Int = 79
    val TOTAL_DOTS: Int = END_DOT + 1 // 80

    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode = {
      if (state.scanlineDot.current > END_DOT) {
        interruptsAndTransition(Draw, state, interrupts)
      } else {
        OamScanner.scan(state)
        OamScan
      }
    }
  }

  case object Draw extends PpuMode(UByte(0x03), canAccessVram = false, canAccessOam = false) {
    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode = this // TODO
  }
}