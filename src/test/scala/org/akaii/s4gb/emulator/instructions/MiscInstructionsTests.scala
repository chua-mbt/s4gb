package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}
import utest.*

object MiscInstructionsTests extends InstructionsTest {

  val tests: Tests = Tests {
    test("NOP") {
      val instruction = Instruction.decode(Array(OpCode.NOP.pattern))
      assert(instruction.toString == "NOP(0x00)")
      verifyInstruction[Instruction.NOP.type](OpCode.NOP.pattern, instruction)

      val finalState = exhaustInstruction(instruction, Instruction.State(Registers(), TestMap()))

      val expectedRegisters = Registers()
      expectedRegisters.pc = instruction.bytes.toUShort
      expectedRegisters.sp = instruction.cycles.toUShort

      // NOP should not change state besides PC and SP
      verifyState(
        finalState, instruction,
        expectedRegisters = expectedRegisters,
        expectedMemory = TestMap()
      )
    }
  }
}
