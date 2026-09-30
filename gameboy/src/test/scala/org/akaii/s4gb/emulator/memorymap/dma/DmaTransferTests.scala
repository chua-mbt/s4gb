package org.akaii.s4gb.emulator.memorymap.dma

import munit.FunSuite
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

class DmaTransferTests extends FunSuite {

  import DmaTransferTests.*

  test("tick advances dot count") {
    val state = activeState()
    DmaTransfer.tick(state, mockMemory, freshOam())
    assertEquals(state.dot, 1)
  }

  test("transfer occurs at exactly 640 dots") {
    val state = activeState()
    val oam = freshOam()

    for (_ <- 1 to 48 * DmaTransfer.DOTS_PER_BYTE) {
      DmaTransfer.tick(state, mockMemory, oam)
    }
    assertEquals(oam.bytes(47), UByte(47))
    assertEquals(oam.bytes(48), UByte(0))

    for (_ <- 1 to 48 * DmaTransfer.DOTS_PER_BYTE) {
      DmaTransfer.tick(state, mockMemory, oam)
    }
    assertEquals(oam.bytes(95), UByte(95))
    assertEquals(oam.bytes(96), UByte(0))

    for (_ <- 1 to DmaTransfer.DOTS_PER_TRANSFER - 96 * DmaTransfer.DOTS_PER_BYTE) {
      DmaTransfer.tick(state, mockMemory, oam)
    }

    assert(!state.isActive)
    assertEquals(state.dot, 0)

    for (i <- 0 until Ppu.OAM_SIZE) {
      assertEquals(oam.bytes(i), UByte(i))
    }
  }
}

object DmaTransferTests {

  private val mockMemory = new MemoryMap {
    override def apply(address: UShort): UByte = UByte(address.toInt & 0xFF)
    override def write(address: UShort, value: UByte): Unit = ()
    override def fetchIfPresent(address: UShort): Option[UByte] = None
  }

  class OamMemory extends MemoryMap {
    val bytes: Array[UByte] = Array.fill(Ppu.OAM_SIZE)(UByte(0))
    override def apply(address: UShort): UByte = bytes(index(address))
    override def write(address: UShort, value: UByte): Unit = bytes(index(address)) = value
    private def index(address: UShort): Int = address.toInt - Ppu.Address.OAM.START.toInt
  }

  def freshOam(): OamMemory = new OamMemory

  def activeState(): DmaState =
    new DmaState(isActive = true, sourceHighByte = UByte(0xC0))
}
