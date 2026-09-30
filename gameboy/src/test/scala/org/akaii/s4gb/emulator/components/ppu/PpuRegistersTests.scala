package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.extensions.byteops.*
import spire.math.{UByte, UShort}

class PpuRegistersTests extends FunSuite with TestEmitters {

  import PpuRegistersTests.dataRegisters

  test("initialize resets registers to power-up values") {
    val ppu = Ppu(Interrupts(), nullEmitter)
    val paletteValues: Map[UShort, UByte] = Map(
      Ppu.Address.BGP -> UByte(0xFC),
      Ppu.Address.OBP0 -> UByte(0xFF),
      Ppu.Address.OBP1 -> UByte(0xFF),
    )
    dataRegisters.foreach { case (_, addr) => ppu.write(addr, UByte(0x3A)) }

    ppu.initialize()

    dataRegisters.foreach { case (name, addr) =>
      assertEquals(ppu(addr), paletteValues.getOrElse(addr, UByte(0)), name)
    }
  }

  dataRegisters.foreach { case (name, addr) =>
    test(s"$name write/read round trip") {
      val ppu = Ppu(Interrupts(), nullEmitter)
      val value = UByte(0x3A)

      ppu.write(addr, value)
      assertEquals(ppu(addr), value)
    }
  }

  test("STAT bit packing (write/read round trip)") {
    val ppu = Ppu(Interrupts(), nullEmitter)
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
    val ppu = Ppu(Interrupts(), nullEmitter)

    val initialLyc = UByte(12)
    ppu.write(Ppu.Address.LYC, initialLyc)

    ppu.write(Ppu.Address.STAT, 0xFF.toUByte)

    assertEquals(ppu(Ppu.Address.LYC), initialLyc)
  }

  test("STAT LYC coincidence bit reflects LY == LYC") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts, nullEmitter)
    ppu.initialize()
    ppu.state.ly = UByte(0x42)
    val lcdStatMask = UByte(1 << Interrupts.Source.LCDStat.bit)

    ppu.write(Ppu.Address.LYC, UByte(0x10))
    assertEquals(ppu(Ppu.Address.STAT) & LcdStatus.Masks.LYC_EQUALS_LY, UByte(0))
    assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG) & lcdStatMask, UByte(0))

    ppu.write(Ppu.Address.LYC, UByte(0x42))
    assertEquals(ppu(Ppu.Address.STAT) & LcdStatus.Masks.LYC_EQUALS_LY, LcdStatus.Masks.LYC_EQUALS_LY)
    assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG) & lcdStatMask, lcdStatMask)
  }

  test("STAT mode bits reflect current PPU mode") {
    val ppu = Ppu(Interrupts(), nullEmitter)
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

  test("LCDC bit packing (write/read round trip)") {
    val ppu = Ppu(Interrupts(), nullEmitter)
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

  test("LY returns current line and writes are ignored") {
    val ppu = Ppu(Interrupts(), nullEmitter)
    ppu.initialize()
    ppu.state.ly = UByte(0x42)

    assertEquals(ppu(Ppu.Address.LY), UByte(0x42))

    ppu.write(Ppu.Address.LY, UByte(0x00))
    assertEquals(ppu.state.ly, UByte(0x42))
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
