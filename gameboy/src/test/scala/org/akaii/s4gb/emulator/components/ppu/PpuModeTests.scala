package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.extensions.byteops.*
import PpuModeTests.*
import spire.math.UByte

class PpuModeTests extends FunSuite {

  test("OamScan transitions to Draw after 80 dots") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts)
    ppu.initialize()

    ppu.state.registers(Ppu.Address.LCDC) = UByte(0x00)
    ppu.state.ly = 0.toUByte
    ppu.state.lcdStatus.ppuMode = PpuMode.OamScan
    ppu.state.scanlineDot.current = 0

    ticksUntilTransition(
      ppu,
      expectedDots = PpuMode.OamScan.TOTAL_DOTS,
      expectedFromMode = PpuMode.OamScan,
      expectedToMode = PpuMode.Draw
    )
  }

  test("HorizontalBlank transitions to VerticalBlank at the end of the visible frame") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts)
    ppu.initialize()

    ppu.state.ly = PpuMode.VISIBLE_SCANLINES_END
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank
    ppu.state.scanlineDot.current = 0

    ticksUntilTransition(
      ppu,
      expectedDots = ScanlineDot.DOTS_PER_LINE,
      expectedFromMode = PpuMode.HorizontalBlank,
      expectedToMode = PpuMode.VerticalBlank
    )

    assertEquals(ppu.state.ly, PpuMode.VBLANK_START_LY)

    val ifReg = interrupts(Interrupts.Address.INTERRUPT_FLAG)
    val vBlankRequested = (ifReg & 0x01.toUByte) != 0.toUByte
    assert(vBlankRequested)
  }

  test("HorizontalBlank on an early scanline transitions to OamScan at the boundary") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts)
    ppu.initialize()

    ppu.state.ly = 0.toUByte
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank

    ppu.state.scanlineDot.current = 0

    ticksUntilTransition(
      ppu,
      expectedDots = ScanlineDot.DOTS_PER_LINE,
      expectedFromMode = PpuMode.HorizontalBlank,
      expectedToMode = PpuMode.OamScan
    )

    assertEquals(ppu.state.ly, 1.toUByte)
  }

  test("VerticalBlank persists for all 10 scanlines before transitioning to OamScan") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts)
    ppu.initialize()

    ppu.state.ly = PpuMode.VBLANK_START_LY
    ppu.state.lcdStatus.ppuMode = PpuMode.VerticalBlank
    ppu.state.scanlineDot.current = 0

    val scanlinesUntilTransition = Ppu.TOTAL_VBLANK_SCANLINES - 1 // 9
    val dotsUntilFinalScanline = scanlinesUntilTransition * ScanlineDot.DOTS_PER_LINE
    // Tick through all but the last VBlank scanline. Mode should remain VBlank.
    var dotsElapsed = 0
    while (dotsElapsed < dotsUntilFinalScanline) {
      ppu.tick()
      dotsElapsed += 1
      assertEquals(ppu.state.lcdStatus.ppuMode, PpuMode.VerticalBlank)
    }
    assertEquals(ppu.state.ly, PpuMode.VBLANK_START_LY + scanlinesUntilTransition.toUByte)

    // Tick through the final VBlank scanline. Should transition to OamScan.
    ticksUntilTransition(
      ppu,
      expectedDots = ScanlineDot.DOTS_PER_LINE,
      expectedFromMode = PpuMode.VerticalBlank,
      expectedToMode = PpuMode.OamScan
    )

    assertEquals(ppu.state.ly, PpuMode.FRAME_WRAP_LY)
  }
}

object PpuModeTests {

  /**
   * Ticks the PPU until the mode transitions from expectedFromMode, verifying total execution duration.
   */
  def ticksUntilTransition(ppu: Ppu, expectedDots: Int, expectedFromMode: PpuMode, expectedToMode: PpuMode): PpuMode = {
    assert(ppu.state.lcdStatus.ppuMode == expectedFromMode)
    var dotsElapsed = 0

    while (ppu.state.lcdStatus.ppuMode == expectedFromMode) {
      ppu.tick()
      dotsElapsed += 1
    }

    assert(dotsElapsed == expectedDots)
    assert(ppu.state.lcdStatus.ppuMode == expectedToMode)
    ppu.state.lcdStatus.ppuMode
  }
}
