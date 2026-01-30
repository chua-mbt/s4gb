package org.akaii.s4gb.emulator.instructions

import munit.*
import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class BitwiseLogicInstructionsTests extends InstructionsTest {
  test("CPL") {
    val instruction = Instruction.decode(Array(OpCode.CPL.pattern))
    verifyInstruction[Instruction.SCF.type](OpCode.CPL.pattern, instruction)

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      },
      registerExpect = regs => {
        regs.a = 0xAA.toUByte // 10101010
        regs.f = 0x60.toUByte // Z=_, N=1, H=1, C=_
      }
    )

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0xAA.toUByte // 10101010
        regs.f = 0xFF.toUByte // Z=1, N=1, H=1, C=1
      },
      registerExpect = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0xFF.toUByte // Z=_, N=1, H=1, C=_
      }
    )
  }
}

