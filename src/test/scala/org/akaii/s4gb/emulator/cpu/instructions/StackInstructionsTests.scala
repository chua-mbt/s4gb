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
      
      val input: Array[UByte] = Array(OpCode.LD_MEM_IMM16_SP.pattern, imm16.loByte, imm16.hiByte)
      val instruction = Instruction.decode(input)

      assertEquals(
        instruction.toString, 
        f"LD_MEM_IMM16_SP(0x${OpCode.LD_MEM_IMM16_SP.pattern.toInt}%02X" +
        f"${imm16.loByte.toInt}%02X${imm16.hiByte.toInt}%02X)"
      )
      verifyInstruction[Instruction.LD_MEM_IMM16_SP](OpCode.LD_MEM_IMM16_SP.pattern, instruction) { ld =>
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
      val opcode: UByte = OpCode.POP_R16STK.setParam(operandParam -> 4)
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
      val opcode: UByte = OpCode.PUSH_R16STK.setParam(operandParam -> 4)
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
          val loByte = if(operandParam == OpCode.Parameters.R16Stack.AF) value.loByte & 0xF0.toUByte else value.loByte
          memory.write(initialSP - 1.toUShort, value.hiByte)
          memory.write(initialSP - 2.toUShort, loByte)
        }
      )
    }
  }

}