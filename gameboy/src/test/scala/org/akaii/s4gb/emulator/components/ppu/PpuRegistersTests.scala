package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

class PpuRegistersTests extends FunSuite {

  import PpuRegistersTests.dataRegisters

  dataRegisters.foreach { case (name, addr) =>
    test(s"$name write/read round trip") {
      val ppu = Ppu()
      val value = UByte(0x3A)

      ppu.write(addr, value)
      assertEquals(ppu(addr), value)
    }
  }

  test("STAT bit packing (write/read round trip)") {
    val ppu = Ppu()
    ppu.initialize()
    val written = 0xFF.toUByte
    ppu.write(Ppu.Address.STAT, written)

    val stat = ppu(Ppu.Address.STAT)

    val lcdStatus = ppu.state.lcdStatus
    assert(lcdStatus.mode2Select)
    assert(lcdStatus.mode1Select)
    assert(lcdStatus.mode0Select)
    assert(lcdStatus.lycSelect)

    assert(lcdStatus.lycEqualsLy)
    assertEquals(lcdStatus.ppuMode, PpuMode.VerticalBlank)

    assertEquals(ppu(Ppu.Address.STAT), 0xF5.toUByte)
  }

  test("STAT does not modify LYC register") {
    val ppu = Ppu()

    val initialLyc = UByte(12)
    ppu.write(Ppu.Address.LYC, initialLyc)

    ppu.write(Ppu.Address.STAT, 0xFF.toUByte)

    assertEquals(ppu(Ppu.Address.LYC), initialLyc)
  }

  test("STAT mode bits reflect current PPU mode") {
    val ppu = Ppu()
    ppu.initialize()

    val modes = Seq(
      PpuMode.HorizontalBlank,
      PpuMode.VerticalBlank,
      PpuMode.OamScan,
      PpuMode.Draw
    )

    modes.foreach { mode =>
      ppu.state.lcdStatus.ppuMode = mode

      val stat = ppu(Ppu.Address.STAT)

      assertEquals(
        stat & LcdStatus.Masks.PPU_MODE,
        mode.statValue & LcdStatus.Masks.PPU_MODE
      )
    }
  }

  test("STAT LYC coincidence bit reflects LY == LYC") {
    // TODO: bit gets set when LY=LYC, and not in other cases, iterate over every LYC until wrap-around
  }

  test("LCDC bit packing (write/read round trip)") {
    val ppu = Ppu()
    ppu.initialize()

    val written = 0xFF.toUByte
    ppu.write(Ppu.Address.LCDC, written)

    val lcdc = ppu(Ppu.Address.LCDC)
    val lcdControl = ppu.state.lcdControl

    assert(lcdControl.lcdEnable)
    assert(lcdControl.windowTileMap)
    assert(lcdControl.windowEnable)
    assert(lcdControl.bgWindowTileData)
    assert(lcdControl.bgTileMap)
    assert(lcdControl.objSize)
    assert(lcdControl.objEnable)
    assert(lcdControl.bgEnable)

    assertEquals(lcdc, 0xFF.toUByte)
  }
}

object PpuRegistersTests {
  private val dataRegisters = Seq(
    ("SCX", Ppu.Address.SCX),
    ("SCY", Ppu.Address.SCY),
    ("WX", Ppu.Address.WX),
    ("WY", Ppu.Address.WY),
    ("LYC", Ppu.Address.LYC),
    ("BGP", Ppu.Address.BGP),
    ("OBP0", Ppu.Address.OBP0),
    ("OBP1", Ppu.Address.OBP1),
  )
}