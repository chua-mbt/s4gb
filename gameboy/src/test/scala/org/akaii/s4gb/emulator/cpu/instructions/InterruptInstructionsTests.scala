package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Cpu.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class InterruptInstructionsTests extends InstructionsTest {
  test("DI") {
    val instruction = Instruction.decode(Array(OpCode.Base.DI.pattern))
    assertEquals(instruction.toString, "DI(0xF3)")
    assertEquals(instruction, Instruction.DI)

    testInstruction(instruction = instruction, setupIME = IMEEnabled, expectedIME = IMEDisabled)
    testInstruction(instruction = instruction, setupIME = IMEEnabling, expectedIME = IMEDisabled)
    testInstruction(instruction = instruction, setupIME = IMEDisabled, expectedIME = IMEDisabled)
  }

  test("EI") {
    val instruction = Instruction.decode(Array(OpCode.Base.EI.pattern))
    assertEquals(instruction.toString, "EI(0xFB)")
    assertEquals(instruction, Instruction.EI)

    testInstruction(instruction = instruction, setupIME = IMEEnabled, expectedIME = IMEEnabled)
    testInstruction(instruction = instruction, setupIME = IMEEnabling, expectedIME = IMEEnabling)
    testInstruction(instruction = instruction, setupIME = IMEDisabled, expectedIME = IMEEnabling)
  }

  test("HALT") {
    val instruction = Instruction.decode(Array(OpCode.Base.HALT.pattern))
    assertEquals(instruction.toString, "HALT(0x76)")
    assertEquals(instruction, Instruction.HALT)

    testInstruction(instruction = instruction, expectedExecutionMode = ExecutionMode.Halted)
  }
}
