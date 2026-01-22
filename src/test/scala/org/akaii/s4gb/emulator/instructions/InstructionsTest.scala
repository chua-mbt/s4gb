package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}
import utest.*

abstract class InstructionsTest extends TestSuite {

  @annotation.tailrec
  final def exhaustInstruction(instruction: Instruction, state: Instruction.State): Instruction.State = {
    if (instruction.execute(state))
      state
    else
      exhaustInstruction(instruction, state.copy(elapsed = state.elapsed + 1))
  }

  protected def verifyInstruction[T <: Instruction](
    opCode: UByte,
    instruction: Instruction
  )(verifications: T => Unit = (_: T) => ()): Unit = {
    assert(instruction.isInstanceOf[T])
    assert(instruction.opCode == opCode)
    assert(instruction.micro.length == instruction.cycles)
    verifications(instruction.asInstanceOf[T])
  }

  protected def verifyState(
    finalState: Instruction.State,
    instruction: Instruction,
    expectedRegisters: Registers,
    expectedMemory: TestMap
  ): Unit = {
    assert(finalState.elapsed == instruction.cycles)
    assert(finalState.elapsed == expectedRegisters.sp.toInt)
    assert(finalState.registers == expectedRegisters)
    assert(finalState.memory == expectedMemory)
    assert(finalState.registers.sp == instruction.cycles.toUShort)
    assert(finalState.registers.pc == instruction.bytes.toUShort)
  }
}
