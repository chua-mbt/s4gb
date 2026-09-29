package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

class PpuTests extends FunSuite with TestEmitters {

  test("tick advances exactly 456 dots per scanline") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts, nullEmitter)
    ppu.initialize()

    ppu.state.ly = PpuMode.VBLANK_START_LY
    ppu.state.lcdStatus.ppuMode = PpuMode.VerticalBlank
    ppu.state.scanlineDot.current = 0
    ppu.state.scanlineDot.cumulative = 0

    var ticks = 0
    while (ticks < ScanlineDot.DOTS_PER_LINE) {
      ppu.tick()
      ticks += 1
    }

    assertEquals(ppu.state.scanlineDot.current, 0)
  }
}
