package org.akaii.s4gb.emulator

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.instructions.OpCode
import spire.math.UByte

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
 * Extensions to help set parameters for OpCode instances in testing
 *
 * Requires targetName disambiguation due to type JVM erasure
 */
extension (opcode: OpCode)
  @scala.annotation.targetName("setParamR8")
  def setParam(params: (OpCode.Parameters.R8, Int)*): UByte =
    params.foldLeft(opcode.pattern) { case (acc, (param, shift)) =>
      acc | (param.ordinal << shift).toUByte
    }

  @scala.annotation.targetName("setParamR16")
  def setParam(params: (OpCode.Parameters.R16, Int)*): UByte =
    params.foldLeft(opcode.pattern) { case (acc, (param, shift)) =>
      acc | (param.ordinal << shift).toUByte
    }