package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.instructions.OpCode
import spire.math.UByte

import scala.language.reflectiveCalls

extension (registers: Registers) {
  def copyTo(target: Registers): Unit = {
    System.arraycopy(registers.underlying, 0, target.underlying, 0, registers.underlying.length)
    target.sp = registers.sp
    target.pc = registers.pc
    target.f = registers.f
  }
}

extension (memory: TestMap) {
  def copyTo(target: TestMap): Unit = {
    System.arraycopy(memory.underlying, 0, target.underlying, 0, memory.underlying.length)
  }
}

/**
 * Extension to help set parameters for OpCode instances in testing
 */
extension (opcode: OpCode)
  def setParam[T <: { def ordinal: Int }](params: (T, Int)*): UByte =
    params.foldLeft(opcode.pattern) { case (acc, (param, shift)) =>
      acc | (param.ordinal << shift).toUByte
    }