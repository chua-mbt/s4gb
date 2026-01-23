package org.akaii.s4gb.emulator.instructions

import munit.*
import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class MiscInstructionsTests extends InstructionsTest {
    test("NOP") {
      val instruction = Instruction.decode(Array(OpCode.NOP.pattern))
      assertEquals(instruction.toString, "NOP(0x00)")
      verifyInstruction[Instruction.NOP.type](OpCode.NOP.pattern, instruction)

      testInstruction(instruction = instruction)
    }
}
