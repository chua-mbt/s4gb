package org.akaii.s4gb.emulator

extension (registers: cpu.Registers) {
  def copyTo(target: cpu.Registers): Unit = {
    System.arraycopy(registers.underlying, 0, target.underlying, 0, registers.underlying.length)
    target.sp = registers.sp
    target.pc = registers.pc
  }
}

extension (memory: TestMap) {
  def copyTo(target: TestMap): Unit = {
    System.arraycopy(memory.underlying, 0, target.underlying, 0, memory.underlying.length)
  }
}