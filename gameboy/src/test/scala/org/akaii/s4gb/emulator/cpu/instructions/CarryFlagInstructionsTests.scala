package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class CarryFlagInstructionsTests extends InstructionsTest {
  test("SCF") {
    val instruction = Instruction.decode(Array(OpCode.Base.SCF.pattern))
    assertEquals(instruction.toString, "SCF(0x37)")
    verifyInstruction[Instruction.SCF.type](OpCode.Base.SCF.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.flags.c = false
        regs.flags.n = true
        regs.flags.h = true
        regs.flags.z = true
      },
      expectedRegister = regs => {
        regs.flags.c = true
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.z = true
      }
    )

    testInstruction(
      instruction,
      setupRegister = regs => regs.flags.c = true,
      expectedRegister = regs => {
        regs.flags.c = true
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.z = false
      }
    )
  }

  test("CCF") {
    val instruction = Instruction.decode(Array(OpCode.Base.CCF.pattern))
    assertEquals(instruction.toString, "CCF(0x3F)")
    verifyInstruction[Instruction.CCF.type](OpCode.Base.CCF.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.flags.c = false
        regs.flags.n = true
        regs.flags.h = true
        regs.flags.z = true
      },
      expectedRegister = regs => {
        regs.flags.c = true
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.z = true
      }
    )

    testInstruction(
      instruction,
      setupRegister = regs => regs.flags.c = true,
      expectedRegister = regs => {
        regs.flags.c = false
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.z = false
      }
    )
  }
}

