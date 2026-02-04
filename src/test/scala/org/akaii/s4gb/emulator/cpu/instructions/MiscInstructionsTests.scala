package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class MiscInstructionsTests extends InstructionsTest {
  test("NOP") {
    val instruction = Instruction.decode(Array(OpCode.NOP.pattern))
    assertEquals(instruction.toString, "NOP(0x00)")
    assertEquals(instruction, Instruction.NOP)

    testInstruction(instruction = instruction)
  }

  test("DAA - flags[N]") {
    val baseA: UByte = 0x15.toUByte
    val instruction = Instruction.decode(Array(OpCode.DAA.pattern))
    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = baseA
        regs.flags.n = true
      },
      expectedRegister = regs => {
        regs.a = baseA
        regs.flags.h = false
      }
    )
  }

  test("DAA - flags[NH]") {
    val baseA: UByte = 0x15.toUByte
    val instruction = Instruction.decode(Array(OpCode.DAA.pattern))
    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = baseA
        regs.flags.n = true
        regs.flags.h = true
      },
      expectedRegister = regs => {
        regs.a = 0x0F.toUByte
        regs.flags.h = false
      }
    )
  }

  test("DAA - flags[NHC]") {
    val baseA: UByte = 0x15.toUByte
    val instruction = Instruction.decode(Array(OpCode.DAA.pattern))
    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = baseA
        regs.flags.n = true
        regs.flags.h = true
        regs.flags.c = true
      },
      expectedRegister = regs => {
        regs.a = 0xAF.toUByte
        regs.flags.h = false
      }
    )
  }

  test("DAA - flags[H]") {
    val baseA: UByte = 0x15.toUByte
    val instruction = Instruction.decode(Array(OpCode.DAA.pattern))
    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = baseA
        regs.flags.h = true
      },
      expectedRegister = regs => {
        regs.a = 0x1B.toUByte
        regs.flags.h = false
      }
    )
  }


  test("DAA - flags[HC]") {
    val baseA: UByte = 0x15.toUByte
    val instruction = Instruction.decode(Array(OpCode.DAA.pattern))
    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = baseA
        regs.flags.h = true
        regs.flags.c = true
      },
      expectedRegister = regs => {
        regs.a = 0x7B.toUByte
        regs.flags.h = false
        regs.flags.c = true
      }
    )
  }

  test("DAA - flags[NH] zero result") {
    val baseA: UByte = 0x06.toUByte
    val instruction = Instruction.decode(Array(OpCode.DAA.pattern))
    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = baseA
        regs.flags.n = true
        regs.flags.h = true
        regs.flags.z = false
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.flags.h = false
        regs.flags.z = true
      }
    )
  }

}
