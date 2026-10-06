package org.akaii.s4gb.emulator.memorymap

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Interrupts.Address.*
import org.akaii.s4gb.emulator.components.Joypad.Address.*
import org.akaii.s4gb.emulator.components.Rom.Address.*
import org.akaii.s4gb.emulator.components.ppu.{PixelEmitter, Ppu}
import org.akaii.s4gb.emulator.components.{Interrupts, Joypad, Rom}
import org.akaii.s4gb.emulator.memorymap.dma.{DmaRegister, DmaState}
import spire.math.{UByte, UShort}

class DispatcherTests extends FunSuite {

  import DispatcherTests.*

  test("throws for unmapped address") {
    val dispatcher = freshDispatcher()
    intercept[IllegalArgumentException] {
      dispatcher(UShort(0xC000))
    }
  }

  test("fetchIfPresent returns None for unmapped address") {
    val dispatcher = freshDispatcher()
    assertEquals(dispatcher.fetchIfPresent(UShort(0xC000)), None)
  }

  test("ROM write is no-op, read returns value") {
    val dispatcher = freshDispatcher()
    dispatcher.write(ROM_START, UByte(0xFF))
    assertEquals(dispatcher(ROM_START), UByte(0x00))
  }

  test("Joypad write/read roundtrip") {
    val dispatcher = freshDispatcher()
    dispatcher.write(JOYPAD, UByte(0x20))
    assertEquals(dispatcher(JOYPAD).toInt & 0x30, 0x20)
  }

  test("Interrupt flag write/read roundtrip - discards upper bits") {
    val dispatcher = freshDispatcher()
    dispatcher.write(INTERRUPT_FLAG, UByte(0xFF))
    val value = dispatcher(INTERRUPT_FLAG)
    assertEquals(value.toInt & 0x1F, 0x1F)
  }

  test("Interrupt enable write/read roundtrip") {
    val dispatcher = freshDispatcher()
    dispatcher.write(INTERRUPT_ENABLE, UByte(0xFF))
    assertEquals(dispatcher(INTERRUPT_ENABLE), UByte(0xFF))
  }

  test("PPU register range boundaries route to PPU") {
    val dispatcher = freshDispatcher()
    dispatcher.write(Ppu.Address.LCDC, UByte(0x91))
    dispatcher.write(Ppu.Address.LYC, UByte(0x42))
    dispatcher.write(Ppu.Address.BGP, UByte(0xE4))
    dispatcher.write(Ppu.Address.WX, UByte(0x07))

    assertEquals(dispatcher(Ppu.Address.LCDC), UByte(0x91))
    assertEquals(dispatcher(Ppu.Address.LYC), UByte(0x42))
    assertEquals(dispatcher(Ppu.Address.BGP), UByte(0xE4))
    assertEquals(dispatcher(Ppu.Address.WX), UByte(0x07))
  }

  test("VRAM range boundaries route to PPU") {
    val dispatcher = freshDispatcher()
    dispatcher.write(Ppu.Address.VRAM.START, UByte(0x12))
    dispatcher.write(Ppu.Address.VRAM.END, UByte(0x34))

    assertEquals(dispatcher(Ppu.Address.VRAM.START), UByte(0x12))
    assertEquals(dispatcher(Ppu.Address.VRAM.END), UByte(0x34))
  }

  test("OAM range boundaries route to PPU") {
    val dispatcher = freshDispatcher()
    dispatcher.write(Ppu.Address.OAM.START, UByte(0x56))
    dispatcher.write(Ppu.Address.OAM.END, UByte(0x78))

    assertEquals(dispatcher(Ppu.Address.OAM.START), UByte(0x56))
    assertEquals(dispatcher(Ppu.Address.OAM.END), UByte(0x78))
  }

  test("DMA register routes to DmaRegister") {
    val dmaState = DmaState()
    val dispatcher = freshDispatcher(dmaState)

    dispatcher.write(DmaRegister.Address.DMA, UByte(0x80))

    assertEquals(dmaState.sourceHighByte, UByte(0x80))
    assert(dmaState.isActive)
    assertEquals(dispatcher(DmaRegister.Address.DMA), Ppu.GARBAGE)
  }

  test("HRAM range boundaries round trip") {
    val dispatcher = freshDispatcher()
    dispatcher.write(Hram.Address.HRAM_START, UByte(0x5A))
    dispatcher.write(Hram.Address.HRAM_END, UByte(0xA5))

    assertEquals(dispatcher(Hram.Address.HRAM_START), UByte(0x5A))
    assertEquals(dispatcher(Hram.Address.HRAM_END), UByte(0xA5))
  }
}

object DispatcherTests {

  private def freshDispatcher(dmaState: DmaState = DmaState()): Dispatcher = {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts, discardingEmitter)
    Dispatcher.withRanges(
      (Rom.Address.ROM_START -> Rom.Address.ROM_END) -> Rom(),
      (Ppu.Address.VRAM.START -> Ppu.Address.VRAM.END) -> ppu,
      (Ppu.Address.OAM.START -> Ppu.Address.OAM.END) -> ppu,
      (Ppu.Address.LCDC -> Ppu.Address.LYC) -> ppu,
      (Ppu.Address.BGP -> Ppu.Address.WX) -> ppu,
      (Joypad.Address.JOYPAD -> Joypad.Address.JOYPAD) -> Joypad(interrupts),
      (DmaRegister.Address.DMA -> DmaRegister.Address.DMA) -> DmaRegister(dmaState),
      (Interrupts.Address.INTERRUPT_FLAG -> Interrupts.Address.INTERRUPT_FLAG) -> interrupts,
      (Interrupts.Address.INTERRUPT_ENABLE -> Interrupts.Address.INTERRUPT_ENABLE) -> interrupts,
      (Hram.Address.HRAM_START -> Hram.Address.HRAM_END) -> Hram(),
    )
  }

  private val discardingEmitter: PixelEmitter = (x: Int, y: Int, color: UByte) => ()
}