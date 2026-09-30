package org.akaii.s4gb.emulator.memorymap

import munit.FunSuite
import spire.math.{UByte, UShort}

class HramTests extends FunSuite {

  import HramTests.*

  test("reads default to zero at start, middle, and end") {
    val hram = Hram()
    assertEquals(hram(Hram.Address.HRAM_START), UByte(0))
    assertEquals(hram(MID), UByte(0))
    assertEquals(hram(Hram.Address.HRAM_END), UByte(0))
  }

  test("write and apply round trip at start, middle, and end") {
    val hram = Hram()
    hram.write(Hram.Address.HRAM_START, UByte(0x11))
    hram.write(MID, UByte(0x22))
    hram.write(Hram.Address.HRAM_END, UByte(0x33))
    assertEquals(hram(Hram.Address.HRAM_START), UByte(0x11))
    assertEquals(hram(MID), UByte(0x22))
    assertEquals(hram(Hram.Address.HRAM_END), UByte(0x33))
  }

  test("addresses hold independent values") {
    val hram = Hram()
    hram.write(Hram.Address.HRAM_START, UByte(0x42))
    hram.write(MID, UByte(0x43))
    assertEquals(hram(Hram.Address.HRAM_START), UByte(0x42))
    assertEquals(hram(MID), UByte(0x43))
  }

  test("rejects address it does not own") {
    val hram = Hram()
    intercept[IllegalArgumentException] {
      hram(UShort(0xFF7F))
    }
    intercept[IllegalArgumentException] {
      hram(UShort(0xFFFF))
    }
    intercept[IllegalArgumentException] {
      hram.write(UShort(0xFFFF), UByte(0x00))
    }
  }
}

object HramTests {

  val MID: UShort = UShort(0xFFBF)
}
