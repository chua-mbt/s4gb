package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.components.*
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.cpu.Cpu
import org.akaii.s4gb.emulator.memorymap.MemoryMap

case class Emulator(cpu: Cpu, ppu: Ppu, timer: Timer) {
  import Emulator.*

  def tick(cycles: Long): Unit = {
    if(cpu.isHardLocked) return
    if(cycles % TCYCLES_PER_MCYCLE == 0) {
      cpu.tick()
    }
    timer.tick()
    ppu.tick()
  }
}

object Emulator {
  private val TCYCLES_PER_MCYCLE: Int = 4
}