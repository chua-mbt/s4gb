package org.akaii.s4gb.emulator.memorymap.dma

import munit.FunSuite
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.memorymap.{Hram, MemoryMap}
import spire.math.{UByte, UShort}

import scala.collection.mutable

class DmaGateTests extends FunSuite {

  import DmaGateTests.*

  test("passes reads through to the bus while DMA is inactive") {
    val (gate, bus) = freshFixture()
    assertEquals(gate(UShort(0x8012)), bus(UShort(0x8012)))
    assertEquals(gate(Ppu.Address.OAM.START), bus(Ppu.Address.OAM.START))
    assertEquals(gate(Ppu.Address.LCDC), bus(Ppu.Address.LCDC))
  }

  test("passes writes through to the bus while DMA is inactive") {
    val (gate, bus) = freshFixture()
    gate.write(UShort(0x8012), UByte(0x42))
    assertEquals(bus(UShort(0x8012)), UByte(0x42))
  }

  test("returns garbage for non-hram reads while DMA is active") {
    val (gate, _) = freshFixture()
    gate.state.isActive = true
    assertEquals(gate(UShort(0x8012)), Ppu.GARBAGE)
    assertEquals(gate(Ppu.Address.OAM.START), Ppu.GARBAGE)
    assertEquals(gate(Ppu.Address.OAM.END), Ppu.GARBAGE)
    assertEquals(gate(Ppu.Address.LCDC), Ppu.GARBAGE)
    assertEquals(gate(UShort(0xFF46)), Ppu.GARBAGE)
  }

  test("drops non-hram writes while DMA is active") {
    val (gate, bus) = freshFixture()
    gate.state.isActive = true
    gate.write(UShort(0x8012), UByte(0x42))
    gate.write(Ppu.Address.OAM.START, UByte(0x42))
    assertEquals(bus(UShort(0x8012)), UByte(0x12))
    assertEquals(bus(Ppu.Address.OAM.START), UByte(0x00))
  }

  test("hram stays readable and writable while DMA is active") {
    val (gate, bus) = freshFixture()
    gate.state.isActive = true
    gate.write(Hram.Address.HRAM_START, UByte(0x77))
    assertEquals(gate(Hram.Address.HRAM_START), UByte(0x77))
    gate.write(Hram.Address.HRAM_END, UByte(0x99))
    assertEquals(bus(Hram.Address.HRAM_END), UByte(0x99))
  }
}

object DmaGateTests {

  class FlatBus extends MemoryMap {
    private val written = mutable.Map.empty[UShort, UByte]
    override def apply(address: UShort): UByte =
      written.getOrElse(address, UByte(address.toInt & 0xFF))
    override def write(address: UShort, value: UByte): Unit = written(address) = value
  }

  def freshFixture(): (DmaGate, FlatBus) = {
    val state = new DmaState()
    val bus = new FlatBus
    (new DmaGate(state, bus), bus)
  }
}
