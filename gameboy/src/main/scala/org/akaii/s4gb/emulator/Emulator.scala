package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.components.*
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.cpu.Cpu

case class Emulator(cpu: Cpu, ppu: Ppu, timer: Timer) {
  import Emulator.*

  /**
   * Advances one m-cycle: four t-cycles of timer and PPU, then one CPU micro-step.
   * Peripherals first means the PPU sees a CPU register write one m-cycle late;
   * Mooneye's `intr_2_0_timing` is what settles whether that is the right way round.
   */
  def tick(): Unit = {
    (0 until TCYCLES_PER_MCYCLE).foreach { _ =>
      timer.tick()
      ppu.tick()
    }
    cpu.tick()
  }
}

object Emulator {
  private val TCYCLES_PER_MCYCLE: Int = 4
}