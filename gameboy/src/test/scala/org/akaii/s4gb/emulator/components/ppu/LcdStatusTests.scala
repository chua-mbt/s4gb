package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import spire.math.UByte

class LcdStatusTests extends FunSuite {

  test("initial state") {
    val status = LcdStatus()
    assertEquals(status.lycSelect, false)
    assertEquals(status.mode2Select, false)
    assertEquals(status.mode1Select, false)
    assertEquals(status.mode0Select, false)
    assertEquals(status.lycEqualsLy, false)
    assertEquals(status.ppuMode, PpuMode.HorizontalBlank)
    assertEquals(status.read(true), UByte(0x00))
  }

  test("initialize resets to power-up state") {
    val status = LcdStatus()
    status.lycSelect = false
    status.mode2Select = true
    status.ppuMode = PpuMode.OamScan

    status.initialize()
    assertEquals(status.lycSelect, true)
    assertEquals(status.mode2Select, false)
    assertEquals(status.mode1Select, false)
    assertEquals(status.mode0Select, false)
    assertEquals(status.lycEqualsLy, true)
    assertEquals(status.ppuMode, PpuMode.VerticalBlank)
    assertEquals(status.read(true), UByte(0x85))
  }

  test("write updates interrupt select bits") {
    val status = LcdStatus()
    status.initialize()

    status.write(UByte(0xF0))
    assertEquals(status.lycSelect, true)
    assertEquals(status.mode2Select, true)
    assertEquals(status.mode1Select, true)
    assertEquals(status.mode0Select, true)
    assertEquals(status.read(true), UByte(0xF5))
  }

  test("write preserves read-only bits") {
    val status = LcdStatus()
    status.initialize()

    status.lycEqualsLy = true
    status.ppuMode = PpuMode.OamScan

    status.write(UByte(0x80))
    assertEquals(status.lycSelect, true)
    assertEquals(status.mode2Select, false)
    assertEquals(status.mode1Select, false)
    assertEquals(status.mode0Select, false)
    assertEquals(status.lycEqualsLy, true)
    assertEquals(status.ppuMode, PpuMode.OamScan)
    assertEquals(status.read(true), UByte(0x86))
  }

  test("read-only bits reflected in read") {
    val status = LcdStatus()
    status.initialize()

    status.lycEqualsLy = true
    status.ppuMode = PpuMode.Draw
    assertEquals(status.read(true), UByte(0x87))

    status.ppuMode = PpuMode.VerticalBlank
    assertEquals(status.read(true), UByte(0x85))
  }

  test("round trip write then read") {
    val combinations = Seq(
      (0x00, 0x05),
      (0x10, 0x15),
      (0x20, 0x25),
      (0x40, 0x45),
      (0x80, 0x85),
      (0xF0, 0xF5),
    )

    combinations.foreach { case (writeVal, expectedRead) =>
      val status = LcdStatus()
      status.initialize()
      status.write(UByte(writeVal))
      assertEquals(status.read(true), UByte(expectedRead), clue = s"write 0x$writeVal%02X")
    }
  }

  test("handles all ppu modes") {
    val status = LcdStatus()
    status.initialize()

    val modes = Seq(
      (PpuMode.HorizontalBlank, 0),
      (PpuMode.VerticalBlank, 1),
      (PpuMode.OamScan, 2),
      (PpuMode.Draw, 3)
    )

    modes.foreach { case (mode, expectedBits) =>
      status.ppuMode = mode
      assertEquals(status.ppuMode, mode)
      assertEquals(status.read(true).toInt & 0x03, expectedBits)
    }
  }

  test("read returns 0 mode bits when LCD disabled") {
    val status = LcdStatus()
    status.initialize()
    status.ppuMode = PpuMode.Draw

    assertEquals(status.read(false), UByte(0x84))  // mode bits forced to 0
  }
}
