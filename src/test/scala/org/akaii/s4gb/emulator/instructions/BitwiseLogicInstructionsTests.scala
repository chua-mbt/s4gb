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
    verifyInstruction[Instruction.CPL.type](OpCode.CPL.pattern, instruction)

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0x00.toUByte // all flags clear
      },
      registerExpect = regs => {
        regs.a = 0xAA.toUByte // 10101010
        regs.f = 0x60.toUByte // N=1, H=1, Z & C unchanged
      }
    )

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0xAA.toUByte // 10101010
        regs.f = 0xF0.toUByte // previous flags set
      },
      registerExpect = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0xF0.toUByte // N=1, H=1, Z & C unchanged
      }
    )
  }

  test("AND_IMM8") {
    val instructionZero = Instruction.decode(Array(OpCode.AND_IMM8.pattern, 0xAA.toUByte))
    verifyInstruction[Instruction.AND_IMM8](OpCode.AND_IMM8.pattern, instructionZero) { and =>
      assertEquals(and.imm8, 0xAA.toUByte)
    }

    testInstruction(
      instructionZero,
      registerSetup = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte
        regs.flags.z = true
        regs.flags.n = false
        regs.flags.h = true
        regs.flags.c = false
      }
    )

    val instructionNonZero = Instruction.decode(Array(OpCode.AND_IMM8.pattern, 0x0F.toUByte))
    verifyInstruction[Instruction.AND_IMM8](OpCode.AND_IMM8.pattern, instructionNonZero) { and =>
      assertEquals(and.imm8, 0x0F.toUByte)
    }

    testInstruction(
      instructionNonZero,
      registerSetup = regs => {
        regs.a = 0xFF.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x0F.toUByte
        regs.flags.z = false
        regs.flags.n = false
        regs.flags.h = true
        regs.flags.c = false
      }
    )
  }

  test("XOR_IMM8") {
    val instructionZero = Instruction.decode(Array(OpCode.XOR_IMM8.pattern, 0xFF.toUByte))
    verifyInstruction[Instruction.XOR_IMM8](OpCode.XOR_IMM8.pattern, instructionZero) { xor =>
      assertEquals(xor.imm8, 0xFF.toUByte)
    }

    testInstruction(
      instructionZero,
      registerSetup = regs => {
        regs.a = 0xFF.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte
        regs.flags.z = true
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.c = false
      }
    )

    val instructionNonZero = Instruction.decode(Array(OpCode.XOR_IMM8.pattern, 0xF0.toUByte))
    verifyInstruction[Instruction.XOR_IMM8](OpCode.XOR_IMM8.pattern, instructionNonZero) { xor =>
      assertEquals(xor.imm8, 0xF0.toUByte)
    }

    testInstruction(
      instructionNonZero,
      registerSetup = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0xFF.toUByte
        regs.flags.z = false
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.c = false
      }
    )
  }

  test("OR_IMM8") {
    val instructionZero = Instruction.decode(Array(OpCode.OR_IMM8.pattern, 0x00.toUByte))
    verifyInstruction[Instruction.OR_IMM8](OpCode.OR_IMM8.pattern, instructionZero) { or =>
      assertEquals(or.imm8, 0x00.toUByte)
    }

    testInstruction(
      instructionZero,
      registerSetup = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte
        regs.flags.z = true
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.c = false
      }
    )

    val instructionNonZero = Instruction.decode(Array(OpCode.OR_IMM8.pattern, 0x02.toUByte))
    verifyInstruction[Instruction.OR_IMM8](OpCode.OR_IMM8.pattern, instructionNonZero) { or =>
      assertEquals(or.imm8, 0x02.toUByte)
    }

    testInstruction(
      instructionNonZero,
      registerSetup = regs => {
        regs.a = 0x01.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x03.toUByte
        regs.flags.z = false
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.c = false
      }
    )
  }
}

