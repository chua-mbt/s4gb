package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.{UByte, UShort}

class StackInstructionsTests extends InstructionsTest {

  test("LD_MEM_IMM16_SP") {
    val testMemoryLocations = Seq(0xC000.toUShort, 0xD000.toUShort, 0xE000.toUShort, 0x0000.toUShort, 0xFFFE.toUShort)

    testMemoryLocations.foreach { memoryLocation =>
      val imm16 = memoryLocation

      val input: Array[UByte] = Array(OpCode.Base.LD_MEM_IMM16_SP.pattern, imm16.loByte, imm16.hiByte)
      val instruction = Instruction.decode(input)

      assertEquals(
        instruction.toString,
        f"LD_MEM_IMM16_SP(0x${OpCode.Base.LD_MEM_IMM16_SP.pattern.toInt}%02X" +
          f"${imm16.loByte.toInt}%02X${imm16.hiByte.toInt}%02X)"
      )
      verifyInstruction[Instruction.LD_MEM_IMM16_SP](OpCode.Base.LD_MEM_IMM16_SP.pattern, instruction) { ld =>
        assertEquals(ld.imm16, imm16)
      }

      val testState = setupTest()
      val testFinal = exhaustInstruction(instruction, testState)

      testInstruction(
        instruction = instruction,
        expectedMemory = memory => {
          memory.write(imm16, UByte(0x00))
          memory.write(imm16 + 1.toUShort, UByte(0x00))
        }
      )
    }
  }

  test("POP_R16STK") {
    val initialSP: UShort = 0xFFFE.toUShort

    forR16StackOpCodeParams { operandParam =>
      val opcode: UByte = OpCode.Base.POP_R16STK.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      verifyInstruction[Instruction.POP_R16STK](opcode, instruction) { pop =>
        assertEquals(pop.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.sp = initialSP
          operandParam match {
            case OpCode.Parameters.R16Stack.BC => regs.bc = 0xFFFF.toUShort
            case OpCode.Parameters.R16Stack.DE => regs.de = 0xFFFF.toUShort
            case OpCode.Parameters.R16Stack.HL => regs.hl = 0xFFFF.toUShort
            case OpCode.Parameters.R16Stack.AF => regs.af = 0xFFFF.toUShort
          }
        },
        setupMemory = (regs, memory) => {
          val value: UShort = 0x1234.toUShort
          memory.write(initialSP, value.loByte)
          memory.write(initialSP + 1.toUShort, value.hiByte)
        },
        expectedRegister = regs => {
          val value: UShort = 0x1234.toUShort
          operandParam match {
            case OpCode.Parameters.R16Stack.BC => regs.bc = value
            case OpCode.Parameters.R16Stack.DE => regs.de = value
            case OpCode.Parameters.R16Stack.HL => regs.hl = value
            case OpCode.Parameters.R16Stack.AF => regs.af = value
          }
          regs.sp = initialSP + 2.toUShort
        }
      )
    }
  }

  test("PUSH_R16STK") {
    val initialSP: UShort = 0xFFFE.toUShort

    forR16StackOpCodeParams { operandParam =>
      val opcode: UByte = OpCode.Base.PUSH_R16STK.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      verifyInstruction[Instruction.PUSH_R16STK](opcode, instruction) { push =>
        assertEquals(push.operand, operandParam)
      }

      val value: UShort = 0x1234.toUShort
      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.sp = initialSP
          operandParam match {
            case OpCode.Parameters.R16Stack.BC => regs.bc = value
            case OpCode.Parameters.R16Stack.DE => regs.de = value
            case OpCode.Parameters.R16Stack.HL => regs.hl = value
            case OpCode.Parameters.R16Stack.AF => regs.af = value
          }
        },
        expectedRegister = regs => regs.sp = initialSP - 2.toUShort,
        expectedMemory = memory => {
          val loByte = if (operandParam == OpCode.Parameters.R16Stack.AF) value.loByte & 0xF0.toUByte else value.loByte
          memory.write(initialSP - 1.toUShort, value.hiByte)
          memory.write(initialSP - 2.toUShort, loByte)
        }
      )
    }
  }

  test("ADD_SP_IMM8") {
    val testCases = Seq(
      // (description, SP initial, imm8 signed, expected SP, expected H, expected C)
      ("Regular positive addition", 0x1000.toUShort, 0x0F, 0x100F.toUShort, false, false),
      ("Half-carry only (H=1, C=0)", 0x100F.toUShort, 0x01, 0x1010.toUShort, true, false),
      ("Carry only (H=0, C=1)", 0x10F0.toUShort, 0x10, 0x1100.toUShort, false, true),
      ("Both half-carry and carry (H=1, C=1)", 0x10F8.toUShort, 0x08, 0x1100.toUShort, true, true),
      ("Negative addition (no wrap)", 0x1008.toUShort, -0x08, 0x1000.toUShort, true, true),
      ("Overflow past 0xFFFF", 0xFFF8.toUShort, 0x10, 0x0008.toUShort, false, true),
      ("Underflow below 0x0000", 0x0008.toUShort, -0x10, 0xFFF8.toUShort, false, false)
    )

    testCases.foreach { case (description, sp, imm, expectedSP, h, c) =>
      val input = Array(OpCode.Base.ADD_SP_IMM8.pattern, imm.toUByte)
      val instruction = Instruction.decode(input)

      verifyInstruction(OpCode.Base.ADD_SP_IMM8.pattern, instruction)

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.sp = sp
          regs.flags.z = true   // should always be cleared
          regs.flags.n = true   // should always be cleared
          regs.flags.h = false
          regs.flags.c = false
        },
        expectedRegister = regs => {
          regs.sp = expectedSP
          regs.flags.z = false
          regs.flags.n = false
          regs.flags.h = h
          regs.flags.c = c
        },
        expectedElapsed = Some(instruction.cycles.maxCost),
        clue = description
      )
    }
  }

  test("LD_HL_ADD_SP_IMM8") {
    val testCases = Seq(
      // (SP initial, imm8 signed, expected HL, expected H, expected C)
      ("Regular positive addition", 0x1000.toUShort, 0x0F, 0x100F.toUShort, false, false),
      ("Half-carry only (H=1, C=0)", 0x100F.toUShort, 0x01, 0x1010.toUShort, true, false),
      ("Carry only (H=0, C=1)", 0x10F0.toUShort, 0x10, 0x1100.toUShort, false, true),
      ("Both half-carry and carry (H=1, C=1)", 0x10F8.toUShort, 0x08, 0x1100.toUShort, true, true),
      ("Negative addition (no wrap)", 0x1008.toUShort, -0x08, 0x1000.toUShort, true, true),
      ("Overflow past 0xFFFF", 0xFFF8.toUShort, 0x10, 0x0008.toUShort, false, true),
      ("Underflow below 0x0000", 0x0008.toUShort, -0x10, 0xFFF8.toUShort, false, false)
    )

    testCases.foreach { case (description, sp, imm, expectedHL, h, c) =>
      val input = Array(OpCode.Base.LD_HL_ADD_SP_IMM8.pattern, imm.toUByte)
      val instr = Instruction.decode(input)
      verifyInstruction(OpCode.Base.LD_HL_ADD_SP_IMM8.pattern, instr)

      testInstruction(
        instr,
        setupRegister = regs => {
          regs.sp = sp
          regs.flags.z = true  // should always be cleared
          regs.flags.n = true
          regs.flags.h = false
          regs.flags.c = false
        },
        expectedRegister = regs => {
          regs.hl = expectedHL
          regs.flags.z = false
          regs.flags.n = false
          regs.flags.h = h
          regs.flags.c = c
        },
        clue = description
      )
    }
  }

  test("LD_SP_HL") {
    val instruction = Instruction.decode(Array(OpCode.Base.LD_SP_HL.pattern))
    verifyInstruction[Instruction.LD_SP_HL.type](OpCode.Base.LD_SP_HL.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0x1234.toUShort,
      expectedRegister = regs => regs.sp = 0x1234.toUShort
    )
  }
}
