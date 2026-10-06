package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.components.*
import org.akaii.s4gb.emulator.components.ppu.{PixelEmitter, Ppu}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.dma.{DmaGate, DmaRegister, DmaState, DmaTransfer}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, Hram, MemoryMap}
import spire.math.UShort

case class Emulator(
  cpu: Cpu,
  ppu: Ppu,
  timer: Timer,
  interrupts: Interrupts,
  joypad: Joypad,
  hram: Hram,
  dma: DmaState,
  bus: MemoryMap
) {
  import Emulator.*

  /**
   * Initializes this machine to DMG power-up state. The timer and DMA state come up already correct.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html]]
   */
  def initialize(): Unit = {
    interrupts.initialize()
    cpu.initialize()
    ppu.initialize()
  }

  /**
   * Advances one m-cycle: four t-cycles of timer, DMA and PPU, then one CPU micro-step.
   * Peripherals first means the PPU sees a CPU register write one m-cycle late;
   * Mooneye's `intr_2_0_timing` is what settles whether that is the right way round.
   *
   * DMA reads the ungated `bus`, not the gated map the CPU sees, so it keeps copying while the CPU
   * is locked out of everything but HRAM.
   */
  def tick(): Unit = {
    (0 until TCYCLES_PER_MCYCLE).foreach { _ =>
      timer.tick()
      DmaTransfer.tick(dma, bus, ppu)
      ppu.tick()
    }
    cpu.tick()
  }
}

object Emulator {
  private val TCYCLES_PER_MCYCLE: Int = 4

  def apply(rom: Rom, io: MemoryMap, emitter: PixelEmitter, config: Config): Emulator = {
    val interrupts = Interrupts()
    val timer = Timer(interrupts)
    val ppu = Ppu(interrupts, emitter)
    val joypad = Joypad(interrupts)
    val hram = Hram()
    val dma = DmaState()

    val bus = Dispatcher.withRanges(
      (Rom.Address.ROM_START, Rom.Address.ROM_END) -> rom,
      (Ppu.Address.VRAM.START, Ppu.Address.VRAM.END) -> ppu,
      (Ppu.Address.OAM.START, Ppu.Address.OAM.END) -> ppu,
      (Ppu.Address.LCDC, Ppu.Address.LYC) -> ppu,
      (Ppu.Address.BGP, Ppu.Address.WX) -> ppu,
      (Joypad.Address.JOYPAD, Joypad.Address.JOYPAD) -> joypad,
      (DmaRegister.Address.DMA, DmaRegister.Address.DMA) -> DmaRegister(dma),
      (Timer.Address.TIMER_START, Timer.Address.TIMER_END) -> timer,
      (Interrupts.Address.INTERRUPT_FLAG, Interrupts.Address.INTERRUPT_FLAG) -> interrupts,
      (Interrupts.Address.INTERRUPT_ENABLE, Interrupts.Address.INTERRUPT_ENABLE) -> interrupts,
      (Hram.Address.HRAM_START, Hram.Address.HRAM_END) -> hram,
      (Unmapped.START, Unmapped.END) -> io,
    )

    Emulator(cpu = Cpu(Cpu.State(Registers(), new DmaGate(dma, bus), config)), ppu, timer, interrupts, joypad, hram, dma, bus)
  }

  object Unmapped {
    val START: UShort = UShort(0xA000)
    val END: UShort = UShort(0xFFFF)
  }
}
