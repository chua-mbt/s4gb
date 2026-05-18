package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.extensions.byteops.toUByte
import spire.math.UByte

/**
 * PPU Modes
 *
 * @see [[https://gbdev.io/pandocs/Rendering.html#highlight=mode#ppu-modes]]
 * @see [[https://gbdev.io/pandocs/Accessing_VRAM_and_OAM.html]]
 */
sealed abstract class PpuMode(val statValue: UByte, val canAccessVram: Boolean, val canAccessOam: Boolean) {
  def tick(state: Ppu.State, interrupts: Interrupts): PpuMode

  def interruptsAndTransition(target: PpuMode, state: Ppu.State, interrupts: Interrupts): PpuMode = {
    val vBlankEntry = target == PpuMode.VerticalBlank
    val hblankLStatInterrupt = target == PpuMode.HorizontalBlank && state.lcdStatus.mode0Select
    val vblankLStatInterrupt = vBlankEntry && state.lcdStatus.mode1Select
    val oamScanLStatInterrupt = target == PpuMode.OamScan && state.lcdStatus.mode2Select
    val lStatInterrupt = hblankLStatInterrupt || vblankLStatInterrupt || oamScanLStatInterrupt

    if(vBlankEntry) interrupts.request(Interrupts.Source.VBlank)
    if(lStatInterrupt) interrupts.request(Interrupts.Source.LCDStat)
    target
  }
}

object PpuMode {
  case object HorizontalBlank extends PpuMode(UByte(0x00), canAccessVram = true, canAccessOam = true) {
    private val END_LY: UByte = 143.toUByte

    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode =
      state.ly match {
        case END_LY => interruptsAndTransition(VerticalBlank, state, interrupts)
        case _ if state.scanlineDot.isBoundary => interruptsAndTransition(OamScan, state, interrupts)
        case _ => interruptsAndTransition(HorizontalBlank, state, interrupts)
      }
  }

  case object VerticalBlank extends PpuMode(UByte(0x01), canAccessVram = true, canAccessOam = true) {
    private val END_LY: UByte = Ppu.SCANLINES_PER_FRAME - 1.toUByte

    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode =
      interruptsAndTransition(if (state.ly == END_LY) OamScan else VerticalBlank, state, interrupts)
  }

  case object OamScan extends PpuMode(UByte(0x02), canAccessVram = true, canAccessOam = false) {
    val END_DOT: Int = 79

    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode = {
      OamScanner.scan(state)
      val nextState = if (state.scanlineDot.current >= END_DOT) Draw else OamScan
      interruptsAndTransition(nextState, state, interrupts)
    }
  }

  case object Draw extends PpuMode(UByte(0x03), canAccessVram = false, canAccessOam = false) {
    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode = this // TODO
  }
}
