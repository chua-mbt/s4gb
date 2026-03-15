package org.akaii.s4gb.emulator.components

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Rom
import org.akaii.s4gb.emulator.components.Rom.Address
import spire.math.{UByte, UShort}

class RomTests extends FunSuite {

  test("read ROM bank 0") {
    val rom = Rom(freshTestData)
    assertEquals(rom(UShort(0x0000)), UByte(0x12))
    assertEquals(rom(UShort(0x3FFF)), UByte(0x34))
  }

  test("read ROM bank 1") {
    val rom = Rom(freshTestData)
    assertEquals(rom(UShort(0x4000)), UByte(0x56))
    assertEquals(rom(UShort(0x7FFF)), UByte(0x78))
  }

  test("load replaces ROM data") {
    val rom = Rom(freshTestData)
    val newData = Array.fill(0x8000)(UByte(0xAA))
    rom.load(newData)

    assertEquals(rom(UShort(0x0000)), UByte(0xAA))
    assertEquals(rom(UShort(0x7FFF)), UByte(0xAA))
  }

  test("write is no-op by default") {
    val rom = Rom(freshTestData)
    rom.write(UShort(0x0000), UByte(0xFF))
    assertEquals(rom(UShort(0x0000)), UByte(0x12))
  }

  test("write throws when configured") {
    val rom = Rom(freshTestData, Rom.OnWrite.Throw)
    intercept[IllegalArgumentException] {
      rom.write(UShort(0x0000), UByte(0xFF))
    }
  }

  test("read throws for invalid address") {
    val rom = Rom(freshTestData)
    intercept[IllegalArgumentException] {
      rom(UShort(0x8000))
    }
  }

  test("write throws for invalid address") {
    val rom = Rom(freshTestData)
    intercept[IllegalArgumentException] {
      rom.write(UShort(0x8000), UByte(0xFF))
    }
  }

  test("boundary - ROM address range") {
    val rom = Rom(freshTestData)
    assertEquals(rom(Address.ROM_START), UByte(0x12))
    assertEquals(rom(Address.ROM_END), UByte(0x78))
  }

  private def freshTestData = {
    val data = Array.fill(0x8000)(UByte(0))
    data(0x0000) = UByte(0x12)
    data(0x3FFF) = UByte(0x34)
    data(0x4000) = UByte(0x56)
    data(0x7FFF) = UByte(0x78)
    data
  }
}
