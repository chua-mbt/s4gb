package org.akaii.s4gb.emulator.memorymap

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Interrupts.Address.*
import org.akaii.s4gb.emulator.components.Rom.Address.*
import org.akaii.s4gb.emulator.components.{Interrupts, Joypad, Rom}
import Joypad.Address.*
import spire.math.{UByte, UShort}

class DispatcherTests extends FunSuite {

  private def freshDispatcher = Dispatcher.withComponents

  test("throws for unmapped address") {
    val dispatcher = freshDispatcher
    intercept[IllegalArgumentException] {
      dispatcher(UShort(0xC000))
    }
  }

  test("fetchIfPresent returns None for unmapped address") {
    val dispatcher = freshDispatcher
    assertEquals(dispatcher.fetchIfPresent(UShort(0xC000)), None)
  }

  test("ROM write is no-op, read returns value") {
    val dispatcher = freshDispatcher
    val data = Array.fill(0x8000)(UByte(0x42))
    dispatcher.write(ROM_START, UByte(0xFF))
    assertEquals(dispatcher(ROM_START), UByte(0x00))
  }

  test("Joypad write/read roundtrip") {
    val dispatcher = freshDispatcher
    dispatcher.write(JOYPAD, UByte(0x20))
    assertEquals(dispatcher(JOYPAD).toInt & 0x30, 0x20)
  }

  test("Interrupt flag write/read roundtrip - discards upper bits") {
    val dispatcher = freshDispatcher
    dispatcher.write(INTERRUPT_FLAG, UByte(0xFF))
    val value = dispatcher(INTERRUPT_FLAG)
    assertEquals(value.toInt & 0x1F, 0x1F)
  }

  test("Interrupt enable write/read roundtrip") {
    val dispatcher = freshDispatcher
    dispatcher.write(INTERRUPT_ENABLE, UByte(0xFF))
    assertEquals(dispatcher(INTERRUPT_ENABLE), UByte(0xFF))
  }

  // TODO: Ppu round-trips
}
