package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.UByte

class ByteArithmeticInstructionsTests extends InstructionsTest {

  test("INC_R8 - normal increment") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"INC_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.INC_R8](opcode, instruction) { inc =>
        assertEquals(inc.operand, operandParam)
      }

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x12.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x13.toUByte
          regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=unchanged
        }
      )
    }
  }

  test("INC_R8 - overflow and zero flag set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0xFF.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=unchanged
        }
      )
    }
  }

  test("INC_R8 - half-carry set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x0F.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x10.toUByte
          regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - normal decrement") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"DEC_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.DEC_R8](opcode, instruction) { dec =>
        assertEquals(dec.operand, operandParam)
      }

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x12.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x11.toUByte
          regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - zero flag set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x01.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0xC0.toUByte // Z=1, N=1, H=1, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - underflow") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0xFF.toUByte
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - half-carry set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x10.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x0F.toUByte
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=unchanged
        }
      )
    }
  }
}
