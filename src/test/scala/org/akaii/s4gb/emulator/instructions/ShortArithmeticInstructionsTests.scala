package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.{TestMap, setParam}
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.UByte

class ShortArithmeticInstructionsTests extends InstructionsTest {

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
        registerSetup = regs => regs(operandParam.toRegister) = 0x1234.toUShort,
        registerExpect = regs => regs(operandParam.toRegister) = 0x1235.toUShort
      )
    }
  }

  test("INC_R16 - overflow") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0xFFFF.toUShort,
        registerExpect = regs => regs(operandParam.toRegister) = 0x0000.toUShort
      )
    }
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
        registerSetup = regs => regs(operandParam.toRegister) = 0x1234.toUShort,
        registerExpect = regs => regs(operandParam.toRegister) = 0x1233.toUShort
      )
    }
  }

  test("DEC_R16 - underflow") {
    forNonSPR16OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R16.setParam(operandParam -> 4)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x0000.toUShort,
        registerExpect = regs => regs(operandParam.toRegister) = 0xFFFF.toUShort
      )
    }
  }

}
