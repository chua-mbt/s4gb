package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.cpu.Cpu

case class Emulator(memory: MemoryMap, cpu: Cpu) {
  def tick(): Unit = {
    if(cpu.isStopped) return
    cpu.tick()
  }
}
