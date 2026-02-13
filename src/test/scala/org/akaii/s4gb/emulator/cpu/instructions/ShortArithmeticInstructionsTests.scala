package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.UByte

class ShortArithmeticInstructionsTests extends InstructionsTest {

  test("ADD_HL_SP - normal addition") {
    val opcode: UByte = OpCode.ADD_HL_SP.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, "ADD_HL_SP(0x39)")
    verifyInstructionOpCode[Instruction.ADD_HL_SP.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.hl = 0x1234.toUShort
        regs.sp = 0x1111.toUShort
        regs.flags.z = true // Z should be unaffected
        regs.flags.n = true // N should be cleared by instruction
        regs.flags.h = true // H should be cleared
        regs.flags.c = true // C should be cleared
      },
      expectedRegister = regs => {
        regs.hl = 0x2345.toUShort
        regs.flags.z = true // unchanged
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.c = false
      }
    )
  }

  test("ADD_HL_SP - half-carry") {
    val opcode: UByte = OpCode.ADD_HL_SP.pattern
    val instruction = Instruction.decode(Array(opcode))

    val (hlValue, spValue, expectedHl) = (0x0800.toUShort, 0x0800.toUShort, 0x1000.toUShort)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.hl = hlValue
        regs.sp = spValue
        regs.flags.n = true
        regs.flags.h = false
        regs.flags.c = false
      },
      expectedRegister = regs => {
        regs.hl = expectedHl
        regs.flags.n = false
        regs.flags.h = true
        regs.flags.c = false
      }
    )
  }

  test("ADD_HL_SP - carry / overflow") {
    val opcode: UByte = OpCode.ADD_HL_SP.pattern
    val instruction = Instruction.decode(Array(opcode))

    val (hlValue, spValue, expectedHl) = (0x8000.toUShort, 0x8000.toUShort, 0x0000.toUShort)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.hl = hlValue
        regs.sp = spValue
        regs.flags.n = true
        regs.flags.h = false
        regs.flags.c = false
      },
      expectedRegister = regs => {
        regs.hl = expectedHl
        regs.flags.n = false
        regs.flags.h = false
        regs.flags.c = true
      }
    )
  }

  test("ADD_HL_R16 - normal addition") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADD_HL_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"ADD_HL_R16(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.ADD_HL_R16](opcode, instruction) { add =>
        assertEquals(add.operand, operandParam)
      }

      val sums =
        if(operandParam == OpCode.Parameters.R16.HL) {
          Seq(
            // operand has to be the same, this is just doubling
            (0x1111.toUShort, 0x1111.toUShort, 0x2222.toUShort),
            (0x1234.toUShort, 0x1234.toUShort, 0x2468.toUShort),
          )
        } else {
          Seq(
            (0x1234.toUShort, 0x1111.toUShort, 0x2345.toUShort),
            (0x1234.toUShort, 0x1234.toUShort, 0x2468.toUShort),
          )
        }

      sums.foreach { case(hlValue, operandValue, expectedSum) =>
        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.hl = hlValue
            regs(operandParam.toRegister) = operandValue
            regs.flags.z = true // Z should be unaffected
            regs.flags.n = true // N should be cleared by instruction
            regs.flags.h = true // H should be cleared
            regs.flags.c = true // C should be cleared
          },
          expectedRegister = regs => {
            regs.hl = expectedSum
            regs.flags.z = true // unchanged
            regs.flags.n = false
            regs.flags.h = false
            regs.flags.c = false
          }
        )
      }
    }
  }

  test("ADD_HL_R16 - half-carry") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADD_HL_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      val (hlValue, opValue, expectedSum) =
        if(operandParam == OpCode.Parameters.R16.HL) {
          (0x0800.toUShort, 0x0800.toUShort, 0x1000.toUShort)
        } else {
          (0x0FFF.toUShort, 0x0001.toUShort, 0x1000.toUShort)
        }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.hl = hlValue
          regs(operandParam.toRegister) = opValue
          regs.flags.n = true
          regs.flags.h = false
          regs.flags.c = false
        },
        expectedRegister = regs => {
          regs.hl = expectedSum
          regs.flags.n = false
          regs.flags.h = true
          regs.flags.c = false
        }
      )
    }
  }

  test("ADD_HL_R16 - carry / overflow") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADD_HL_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      val (hlValue, opValue, expectedSum) =
        if (operandParam == OpCode.Parameters.R16.HL) {
          (0x8000.toUShort, 0x8000.toUShort, 0x0000.toUShort)
        } else {
          (0xFFFF.toUShort, 0x0001.toUShort, 0x0000.toUShort)
        }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.hl = hlValue
          regs(operandParam.toRegister) = opValue
          regs.flags.n = true
          regs.flags.h = false
          regs.flags.c = false
        },
        expectedRegister = regs => {
          regs.hl = expectedSum
          regs.flags.n = false
          regs.flags.h = operandParam != OpCode.Parameters.R16.HL
          regs.flags.c = true
        }
      )
    }
  }

  test("INC_SP - normal increment") {
    val opcode: UByte = OpCode.INC_SP.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, "INC_SP(0x33)")
    verifyInstructionOpCode[Instruction.INC_SP.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.sp = 0x1234.toUShort,
      expectedRegister = regs => regs.sp = 0x1235.toUShort
    )
  }

  test("INC_SP - overflow") {
    val opcode: UByte = OpCode.INC_SP.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.sp = 0xFFFF.toUShort,
      expectedRegister = regs => regs.sp = 0x0000.toUShort
    )
  }

  test("INC_R16 - normal increment") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"INC_R16(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.INC_R16](opcode, instruction) { inc =>
        assertEquals(inc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x1234.toUShort,
        expectedRegister = regs => regs(operandParam.toRegister) = 0x1235.toUShort
      )
    }
  }

  test("INC_R16 - overflow") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0xFFFF.toUShort,
        expectedRegister = regs => regs(operandParam.toRegister) = 0x0000.toUShort
      )
    }
  }

  test("DEC_SP - normal decrement") {
    val opcode: UByte = OpCode.DEC_SP.pattern
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, "DEC_SP(0x3B)")
      verifyInstructionOpCode[Instruction.DEC_SP.type](opcode, instruction)

      testInstruction(
        instruction,
        setupRegister = regs => regs.sp = 0x1234.toUShort,
        expectedRegister = regs => regs.sp = 0x1233.toUShort
      )
  }

  test("DEC_SP - underflow") {
    val opcode: UByte = OpCode.DEC_SP.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.sp = 0x0000.toUShort,
      expectedRegister = regs => regs.sp = 0xFFFF.toUShort
    )
  }

  test("DEC_R16 - normal decrement") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"DEC_R16(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.DEC_R16](opcode, instruction) { dec =>
        assertEquals(dec.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x1234.toUShort,
        expectedRegister = regs => regs(operandParam.toRegister) = 0x1233.toUShort
      )
    }
  }

  test("DEC_R16 - underflow") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x0000.toUShort,
        expectedRegister = regs => regs(operandParam.toRegister) = 0xFFFF.toUShort
      )
    }
  }

}
