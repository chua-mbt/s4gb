package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

class PpuModeTests extends FunSuite {

  /**
   * Helper to execute PPU ticks until the mode transitions, verifying total execution duration.
   */
  private def ticksUntilTransition(ppu: Ppu, expectedDots: Int): PpuMode = {
    val initialMode = ppu.state.lcdStatus.ppuMode
    var dotsElapsed = 0

    while (ppu.state.lcdStatus.ppuMode == initialMode) {
      ppu.tick()
      dotsElapsed += 1
    }

    assertEquals(dotsElapsed, expectedDots, s"Mode ${initialMode} did not run for the expected duration")
    ppu.state.lcdStatus.ppuMode
  }

  test("HorizontalBlank transitions to VerticalBlank at the end of the visible frame") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts)
    ppu.initialize()

    ppu.state.ly = PpuMode.VISIBLE_SCANLINES_END
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank

    val hBlankStartDot = 252
    ppu.state.scanlineDot.current = hBlankStartDot
    val expectedDots = ScanlineDot.DOTS_PER_LINE - hBlankStartDot // 204 dots

    val nextMode = ticksUntilTransition(ppu, expectedDots)

    assertEquals(nextMode, PpuMode.VerticalBlank)

    // Verify VBlank Interrupt triggered
    val ifReg = interrupts(Interrupts.Address.INTERRUPT_FLAG)
    val vBlankRequested = (ifReg & 0x01.toUByte) != 0.toUByte
    assert(vBlankRequested, "VBlank interrupt flag was not set upon entry")
  }

  test("HorizontalBlank on an early scanline transitions to OamScan at the boundary") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts)
    ppu.initialize()

    ppu.state.ly = 0.toUByte
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank

    val hBlankStartDot = 252
    ppu.state.scanlineDot.current = hBlankStartDot
    val expectedDots = ScanlineDot.DOTS_PER_LINE - hBlankStartDot // 204 dots

    val nextMode = ticksUntilTransition(ppu, expectedDots)

    assertEquals(ppu.state.ly, 1.toUByte)
    assertEquals(nextMode, PpuMode.OamScan)
  }

  test("VerticalBlank on the final scanline transitions back to OamScan and wraps LY") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts)
    ppu.initialize()

    val finalVBlankScanline = Ppu.SCANLINES_PER_FRAME - 1.toUByte // 153
    ppu.state.ly = finalVBlankScanline
    ppu.state.lcdStatus.ppuMode = PpuMode.VerticalBlank
    ppu.state.scanlineDot.current = 0

    val nextMode = ticksUntilTransition(ppu, ScanlineDot.DOTS_PER_LINE)

    assertEquals(ppu.state.ly, PpuMode.FRAME_WRAP_LY)
    assertEquals(nextMode, PpuMode.OamScan)
  }
}