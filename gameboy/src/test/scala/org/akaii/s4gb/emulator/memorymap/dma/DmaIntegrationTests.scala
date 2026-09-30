package org.akaii.s4gb.emulator.memorymap.dma

import munit.FunSuite
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.components.{Interrupts, Joypad, Rom}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, Hram, MemoryMap}
import spire.math.{UByte, UShort}

import scala.collection.mutable

class DmaIntegrationTests extends FunSuite {

  import DmaIntegrationTests.*

  test("register write locks everything but hram before ticking starts") {
    val fixture = freshFixture()
    val ifBefore = fixture.interrupts(Interrupts.Address.INTERRUPT_FLAG)
    fixture.register.write(DmaRegister.Address.DMA, UByte(0x80))

    assert(fixture.state.isActive)
    assertEquals(fixture.state.sourceHighByte, UByte(0x80))

    lockedAddresses.foreach(address => assertEquals(fixture.gate(address), Ppu.GARBAGE))
    cpuBlockedAddresses.foreach { address =>
      assertEquals(fixture.gate(address), Ppu.GARBAGE)
      fixture.gate.write(address, UByte(0xFF))
    }

    lockedAddresses.foreach(address => assertEquals(fixture.oam(address), UByte(0)))
    assertEquals(fixture.rom(ROM_TEST), UByte(0x10))
    assertEquals(fixture.source(SOURCE_START), UByte(0))
    assertEquals(fixture.interrupts(Interrupts.Address.INTERRUPT_FLAG), ifBefore)

    fixture.gate.write(Hram.Address.HRAM_START, UByte(0x77))
    assertEquals(fixture.gate(Hram.Address.HRAM_START), UByte(0x77))
  }

  test("transfer reads source and writes oam while both are blocked for the cpu") {
    val fixture = freshFixture()
    fixture.register.write(DmaRegister.Address.DMA, UByte(0x80))

    assertEquals(fixture.gate(SOURCE_START), Ppu.GARBAGE)
    assertEquals(fixture.gate(Ppu.Address.OAM.START), Ppu.GARBAGE)

    for (_ <- 1 to DmaTransfer.DOTS_PER_BYTE) {
      DmaTransfer.tick(fixture.state, fixture.source, fixture.oam)
    }

    assertEquals(fixture.oam(Ppu.Address.OAM.START), fixture.source(SOURCE_START))
    assertEquals(fixture.gate(SOURCE_START), Ppu.GARBAGE)
    assertEquals(fixture.gate(Ppu.Address.OAM.START), Ppu.GARBAGE)

    fixture.gate.write(Hram.Address.HRAM_START, UByte(0x55))
    assertEquals(fixture.gate(Hram.Address.HRAM_START), UByte(0x55))
  }

  test("transfer bypasses the gate, copies all bytes, and releases the bus") {
    val fixture = freshFixture()
    fixture.register.write(DmaRegister.Address.DMA, UByte(0x80))

    for (_ <- 1 to 60 * DmaTransfer.DOTS_PER_BYTE) {
      DmaTransfer.tick(fixture.state, fixture.source, fixture.oam)
    }

    assert(fixture.state.isActive)
    lockedAddresses.foreach(address => assertEquals(fixture.gate(address), Ppu.GARBAGE))
    assertEquals(fixture.oam(UShort(0xFE3C)), UByte(0))

    fixture.gate.write(UShort(0xFE05), UByte(0xFF))
    assertEquals(fixture.oam(UShort(0xFE05)), UByte(5))

    fixture.gate.write(Hram.Address.HRAM_END, UByte(0x66))
    assertEquals(fixture.gate(Hram.Address.HRAM_END), UByte(0x66))

    for (_ <- 1 to DmaTransfer.DOTS_PER_TRANSFER - 60 * DmaTransfer.DOTS_PER_BYTE) {
      DmaTransfer.tick(fixture.state, fixture.source, fixture.oam)
    }

    assert(!fixture.state.isActive)
    assertEquals(fixture.state.dot, 0)

    for (i <- 0 until Ppu.OAM_SIZE) {
      assertEquals(fixture.oam.bytes(i), UByte(i))
    }

    lockedAddresses.foreach(address => assertEquals(fixture.gate(address), fixture.oam(address)))
    assertEquals(fixture.gate(ROM_TEST), fixture.rom(ROM_TEST))
    assertEquals(fixture.gate(SOURCE_START), fixture.source(SOURCE_START))

    fixture.gate.write(Interrupts.Address.INTERRUPT_FLAG, UByte(0x1F))
    assertEquals(fixture.gate(Interrupts.Address.INTERRUPT_FLAG).toInt & 0x1F, 0x1F)

    fixture.gate.write(Joypad.Address.JOYPAD, UByte(0x20))
    assertEquals(fixture.gate(Joypad.Address.JOYPAD).toInt & 0x30, 0x20)

    fixture.gate.write(UShort(0xFE10), UByte(0x42))
    assertEquals(fixture.oam(UShort(0xFE10)), UByte(0x42))
  }
}

object DmaIntegrationTests {

  val ROM_TEST: UShort = UShort(0x0010)
  val SOURCE_START: UShort = UShort(0x8000)
  val SOURCE_END: UShort = UShort(0x809F)

  val lockedAddresses: List[UShort] =
    List(Ppu.Address.OAM.START, UShort(0xFE4F), UShort(0xFE50), Ppu.Address.OAM.END)

  val cpuBlockedAddresses: List[UShort] =
    List(
      ROM_TEST,
      SOURCE_START,
      Joypad.Address.JOYPAD,
      Interrupts.Address.INTERRUPT_FLAG,
      Interrupts.Address.INTERRUPT_ENABLE,
    )

  class PatternMemory extends MemoryMap {
    private val written = mutable.Map.empty[UShort, UByte]
    override def apply(address: UShort): UByte =
      written.getOrElse(address, UByte(address.toInt & 0xFF))
    override def write(address: UShort, value: UByte): Unit = written(address) = value
  }

  class OamMemory extends MemoryMap {
    val bytes: Array[UByte] = Array.fill(Ppu.OAM_SIZE)(UByte(0))
    override def apply(address: UShort): UByte = bytes(index(address))
    override def write(address: UShort, value: UByte): Unit = bytes(index(address)) = value
    private def index(address: UShort): Int = address.toInt - Ppu.Address.OAM.START.toInt
  }

  case class Fixture(
    register: DmaRegister,
    gate: DmaGate,
    state: DmaState,
    source: PatternMemory,
    oam: OamMemory,
    rom: PatternMemory,
    interrupts: Interrupts,
  )

  def freshFixture(): Fixture = {
    val state = new DmaState()
    val oam = new OamMemory
    val rom = new PatternMemory
    val source = new PatternMemory
    val interrupts = Interrupts()
    val bus = Dispatcher.withRanges(
      (Rom.Address.ROM_START -> Rom.Address.ROM_END) -> rom,
      (Ppu.Address.OAM.START -> Ppu.Address.OAM.END) -> oam,
      (SOURCE_START -> SOURCE_END) -> source,
      (Joypad.Address.JOYPAD -> Joypad.Address.JOYPAD) -> Joypad(interrupts),
      (Interrupts.Address.INTERRUPT_FLAG -> Interrupts.Address.INTERRUPT_FLAG) -> interrupts,
      (Interrupts.Address.INTERRUPT_ENABLE -> Interrupts.Address.INTERRUPT_ENABLE) -> interrupts,
      (Hram.Address.HRAM_START -> Hram.Address.HRAM_END) -> Hram(),
    )
    Fixture(new DmaRegister(state), new DmaGate(state, bus), state, source, oam, rom, interrupts)
  }
}
