package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.components.*
import org.akaii.s4gb.emulator.components.ppu.{PixelEmitter, Ppu}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.dma.{DmaGate, DmaRegister, DmaState, DmaTransfer}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, MemoryMap, StorageMap}

case class Emulator(
  cpu: Cpu,
  ppu: Ppu,
  timer: Timer,
  interrupts: Interrupts,
  joypad: Joypad,
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

  def apply(rom: Rom, emitter: PixelEmitter, config: Config): Emulator = {
    val interrupts = Interrupts()
    val timer = Timer(interrupts)
    val ppu = Ppu(interrupts, emitter)
    val joypad = Joypad(interrupts)
    val dma = DmaState()

    val serial = StorageMap(StorageMap.Address.SERIAL_START, StorageMap.Address.SERIAL_END)
    val externalRam = StorageMap(StorageMap.Address.EXTERNAL_RAM_START, StorageMap.Address.EXTERNAL_RAM_END)
    val wram0 = StorageMap(StorageMap.Address.WRAM_0_START, StorageMap.Address.WRAM_0_END)
    val wram1 = StorageMap(StorageMap.Address.WRAM_1_START, StorageMap.Address.WRAM_1_END)
    val echoRam = StorageMap(StorageMap.Address.ECHO_RAM_START, StorageMap.Address.ECHO_RAM_END)
    val notUsable = StorageMap(StorageMap.Address.NOT_USABLE_START, StorageMap.Address.NOT_USABLE_END)
    val hram = StorageMap(StorageMap.Address.HRAM_START, StorageMap.Address.HRAM_END)
    val unusedIo03 = StorageMap(StorageMap.Address.UNUSED_IO_03_START, StorageMap.Address.UNUSED_IO_03_END)
    val unusedIo08 = StorageMap(StorageMap.Address.UNUSED_IO_08_START, StorageMap.Address.UNUSED_IO_08_END)
    val audio = StorageMap(StorageMap.Address.AUDIO_START, StorageMap.Address.AUDIO_END)
    val unusedIo4C = StorageMap(StorageMap.Address.UNUSED_IO_4C_START, StorageMap.Address.UNUSED_IO_4C_END)

    val bus = Dispatcher.withRanges(
      (Rom.Address.ROM_START, Rom.Address.ROM_END) -> rom,
      (Ppu.Address.VRAM.START, Ppu.Address.VRAM.END) -> ppu,
      (externalRam.start, externalRam.end) -> externalRam,
      (wram0.start, wram0.end) -> wram0,
      (wram1.start, wram1.end) -> wram1,
      (echoRam.start, echoRam.end) -> echoRam,
      (Ppu.Address.OAM.START, Ppu.Address.OAM.END) -> ppu,
      (notUsable.start, notUsable.end) -> notUsable,
      (Joypad.Address.JOYPAD, Joypad.Address.JOYPAD) -> joypad,
      (StorageMap.Address.SERIAL_START, StorageMap.Address.SERIAL_END) -> serial,
      (unusedIo03.start, unusedIo03.end) -> unusedIo03,
      (Timer.Address.TIMER_START, Timer.Address.TIMER_END) -> timer,
      (unusedIo08.start, unusedIo08.end) -> unusedIo08,
      (Interrupts.Address.INTERRUPT_FLAG, Interrupts.Address.INTERRUPT_FLAG) -> interrupts,
      (audio.start, audio.end) -> audio,
      (Ppu.Address.LCDC, Ppu.Address.LYC) -> ppu,
      (DmaRegister.Address.DMA, DmaRegister.Address.DMA) -> DmaRegister(dma),
      (Ppu.Address.BGP, Ppu.Address.WX) -> ppu,
      (unusedIo4C.start, unusedIo4C.end) -> unusedIo4C,
      (StorageMap.Address.HRAM_START, StorageMap.Address.HRAM_END) -> hram,
      (Interrupts.Address.INTERRUPT_ENABLE, Interrupts.Address.INTERRUPT_ENABLE) -> interrupts,
    )

    Emulator(cpu = Cpu(Cpu.State(Registers(), new DmaGate(dma, bus), config)), ppu, timer, interrupts, joypad, dma, bus)
  }
}
