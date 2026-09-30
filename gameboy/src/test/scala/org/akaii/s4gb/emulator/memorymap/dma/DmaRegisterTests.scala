package org.akaii.s4gb.emulator.memorymap.dma

import munit.FunSuite
import org.akaii.s4gb.emulator.components.ppu.Ppu
import spire.math.{UByte, UShort}

class DmaRegisterTests extends FunSuite {

  test("write latches source high byte and marks transfer active") {
    val state = new DmaState()
    val dma = new DmaRegister(state)

    dma.write(DmaRegister.Address.DMA, UByte(0xC0))

    assertEquals(state.sourceHighByte, UByte(0xC0))
    assert(state.isActive)
  }

  test("write and apply round trip for DMA returns garbage") {
    val state = new DmaState()
    val dma = new DmaRegister(state)

    dma.write(DmaRegister.Address.DMA, UByte(0xC0))

    assertEquals(dma(DmaRegister.Address.DMA), Ppu.GARBAGE)
  }

  test("second write replaces latched source") {
    val state = new DmaState()
    val dma = new DmaRegister(state)

    dma.write(DmaRegister.Address.DMA, UByte(0xC0))
    dma.write(DmaRegister.Address.DMA, UByte(0xD0))

    assertEquals(state.sourceHighByte, UByte(0xD0))
  }

  test("rejects address it does not own") {
    val dma = new DmaRegister(new DmaState())

    intercept[IllegalArgumentException] {
      dma.write(UShort(0xFF40), UByte(0x00))
    }
    intercept[IllegalArgumentException] {
      dma(UShort(0xFF40))
    }
  }
}
