package org.akaii.s4gb.emulator.components

import munit.FunSuite
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.components.Interrupts
import spire.math.UByte

class InterruptsTests extends FunSuite {

  test("initial state") {
    val interrupts = Interrupts()
    assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG), Interrupts.Masks.IGNORED)
    assertEquals(interrupts(Interrupts.Address.INTERRUPT_ENABLE), UByte(0))
    assertEquals(interrupts.toString, "Interrupts()")
  }

  test("request and clear round-trip on IF") {
    val interrupts = Interrupts()
    Interrupts.Source.values.foreach { source =>
      interrupts.request(source)
      val written = interrupts(Interrupts.Address.INTERRUPT_FLAG)
      assertEquals(written, UByte(1 << source.bit) | Interrupts.Masks.IGNORED)
      interrupts.clear(source)
      val cleared = interrupts(Interrupts.Address.INTERRUPT_FLAG)
      assertEquals(cleared, Interrupts.Masks.IGNORED)
    }
  }

  test("multiple requests accumulate") {
    val interrupts = Interrupts()
    interrupts.request(Interrupts.Source.Joypad)
    interrupts.request(Interrupts.Source.Timer)
    val flag = interrupts(Interrupts.Address.INTERRUPT_FLAG)
    assertEquals(flag, UByte((1 << 4) | (1 << 2)) | Interrupts.Masks.IGNORED)
    assertEquals(interrupts.toString, "Interrupts(0xFF0F=0x14)")
  }

  test("IE register unaffected by requests") {
    val interrupts = Interrupts()
    interrupts.request(Interrupts.Source.Joypad)
    val ie = interrupts(Interrupts.Address.INTERRUPT_ENABLE)
    assertEquals(ie, UByte(0))
  }

  test("write and apply round trip for INTERRUPT_ENABLE") {
    val interrupts = Interrupts()
    interrupts.write(Interrupts.Address.INTERRUPT_ENABLE, UByte(0xAB))
    val read = interrupts(Interrupts.Address.INTERRUPT_ENABLE)
    assertEquals(read, UByte(0xAB))
  }

  test("write and apply round trip for INTERRUPT_FLAG") {
    val interrupts = Interrupts()
    interrupts.write(Interrupts.Address.INTERRUPT_FLAG, UByte(0xCD))
    val read = interrupts(Interrupts.Address.INTERRUPT_FLAG)
    assertEquals(read, UByte(0xED))
  }

  test("writing upper bits of IF ignored") {
    val interrupts = Interrupts()
    interrupts.write(Interrupts.Address.INTERRUPT_FLAG, UByte(0x18))
    val read = interrupts(Interrupts.Address.INTERRUPT_FLAG)
    assertEquals(read, UByte(0xF8))
  }

  test("writing zero to IF") {
    val interrupts = Interrupts()
    interrupts.write(Interrupts.Address.INTERRUPT_FLAG, UByte(0x00))
    val read = interrupts(Interrupts.Address.INTERRUPT_FLAG)
    assertEquals(read, Interrupts.Masks.IGNORED)
  }

  test("source prioritization") {
    val sources = Interrupts.Source.values
    sources.foreach { expected =>
      val ifValue = sources
        .filter(s => s.bit >= expected.bit) // all interrupts at or below this priority
        .map(s => 1 << s.bit) // mask the bits
        .reduce(_ | _) // combine them into a single byte
        .toUByte

      val result = Interrupts.Source.highestPriority(ifValue)
      assertEquals(result, expected)
    }
  }
}
