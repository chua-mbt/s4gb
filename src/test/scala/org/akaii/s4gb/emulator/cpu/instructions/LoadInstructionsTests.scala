package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.{UByte, UShort}

class LoadInstructionsTests extends InstructionsTest {
  test("LD_SP_IMM16") {
    val imm16 = 0x1234.toUShort
    val opcode: UByte = OpCode.LD_SP_IMM16.pattern
    val immLo: UByte = imm16.registerLoByte
    val immHi: UByte = imm16.registerHiByte
    val input: Array[UByte] = Array(opcode, immLo, immHi)
    val instruction = Instruction.decode(input)

    assertEquals(instruction.toString, f"LD_SP_IMM16(0x${opcode.toInt}%02X${immLo.toInt}%02X${immHi.toInt}%02X)")
    verifyInstruction[Instruction.LD_SP_IMM16](opcode, instruction) { ld =>
      assertEquals(ld.imm16, imm16)
    }

    testInstruction(
      instruction = instruction,
      expectedRegister = registers => registers.sp = imm16
    )
  }

  test("LD_R16_IMM16") {
    val imm16 = 0x1234.toUShort
    forNonSPR16OpCodeParams { destParam =>
      val opcode: UByte = OpCode.LD_R16_IMM16.setParam(destParam -> 4)
      val immLo: UByte = imm16.registerLoByte
      val immHi: UByte = imm16.registerHiByte

      val input: Array[UByte] = Array(opcode, immLo, immHi)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R16_IMM16(0x${opcode.toInt}%02X${immLo.toInt}%02X${immHi.toInt}%02X)")
      verifyInstruction[Instruction.LD_R16_IMM16](opcode, instruction) { ld =>
        assertEquals(ld.imm16, imm16)
        assertEquals(ld.dest, destParam)
      }

      testInstruction(
        instruction = instruction,
        expectedRegister = registers => registers(destParam.toRegister) = imm16
      )
    }
  }

  test("LD_R16MEM_A") {
    forNonSPR16OpCodeParams { destRefParam =>
      val opcode: UByte = OpCode.LD_R16MEM_A.setParam(destRefParam -> 4)
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R16MEM_A(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_R16MEM_A](opcode, instruction) { ld =>
        assertEquals(ld.destRef, destRefParam)
      }

      testInstruction(
        instruction = instruction,
        setupRegister = registers => {
          registers.a = 0x42.toUByte
          registers(destRefParam.toRegister) = 0xC000.toUShort
        },
        expectedMemory = memory => {
          memory.write(0xC000.toUShort, 0x42.toUByte)
        }
      )
    }
  }

  test("LD_A_R16MEM") {
    forNonSPR16OpCodeParams { srcParam =>
      val opcode: UByte = OpCode.LD_A_R16MEM.setParam(srcParam -> 4)
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_A_R16MEM(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_A_R16MEM](opcode, instruction) { ld =>
        assertEquals(ld.srcRef, srcParam)
      }

      testInstruction(
        instruction = instruction,
        setupRegister = registers => registers(srcParam.toRegister) = 0xC000.toUShort,
        setupMemory = (registers, memory) => memory.write(registers(srcParam.toRegister), 0x42.toUByte),
        expectedRegister = _.a = 0x42.toUByte
      )
    }
  }

  test("LD_MEM_HL_IMM8") {
    val imm8: UByte = 0x42.toUByte
    val opcode: UByte = OpCode.LD_MEM_HL_IMM8.pattern
    val input: Array[UByte] = Array(opcode, imm8)
    val instruction = Instruction.decode(input)

    assertEquals(instruction.toString, f"LD_MEM_HL_IMM8(0x${opcode.toInt}%02X${imm8.toInt}%02X)")
    verifyInstruction[Instruction.LD_MEM_HL_IMM8](opcode, instruction) { ld =>
      assertEquals(ld.imm8, imm8)
    }

    testInstruction(
      instruction = instruction,
      setupRegister = registers => registers.hl = 0xC000.toUShort,
      expectedMemory = memory => memory.write(0xC000.toUShort, imm8)
    )
  }

  test("LD_R8_IMM8") {
    val imm8: UByte = 0x42.toUByte
    forNonMemHLR8OpCodeParams { destParam =>
      val opcode: UByte = OpCode.LD_R8_IMM8.setParam(destParam -> 3)
      val input: Array[UByte] = Array(opcode, imm8)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R8_IMM8(0x${opcode.toInt}%02X${imm8.toInt}%02X)")
      verifyInstruction[Instruction.LD_R8_IMM8](opcode, instruction) { ld =>
        assertEquals(ld.dest, destParam)
        assertEquals(ld.imm8, imm8)
      }

      testInstruction(
        instruction = instruction,
        expectedRegister = registers => registers(destParam.toRegister) = imm8
      )
    }
  }

  test("LD_MEM_HL_R8") {
    forNonMemHLR8OpCodeParams { srcParam =>
      val opcode: UByte = OpCode.LD_MEM_HL_R8.setParam(srcParam -> 0)
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_MEM_HL_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_MEM_HL_R8](opcode, instruction) { ld =>
        assertEquals(ld.src, srcParam)
      }

      val value: UByte = srcParam match {
        case OpCode.Parameters.R8.H => 0xC0.toUByte // H is high portion of HL, which is the target address 0x[C0]00
        case OpCode.Parameters.R8.L => 0x00.toUByte // L is low portion of HL, which is the target address 0xC0[00]
        case _ => 0x42.toUByte
      }

      testInstruction(
        instruction = instruction,
        setupRegister = registers => {
          registers(srcParam.toRegister) = value
          registers.hl = 0xC000.toUShort
        },
        expectedMemory = memory => memory.write(0xC000.toUShort, value)
      )
    }
  }

  test("LD_R8_MEM_HL") {
    val value: UByte = 0x42.toUByte
    forNonMemHLR8OpCodeParams { destParam =>
      val opcode: UByte = OpCode.LD_R8_MEM_HL.setParam(destParam -> 3)
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R8_MEM_HL(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_R8_MEM_HL](opcode, instruction) { ld =>
        assertEquals(ld.dest, destParam)
      }

      testInstruction(
        instruction = instruction,
        setupRegister = registers => registers.hl = 0xC000.toUShort,
        setupMemory = (registers, memory) => memory.write(0xC000.toUShort, value),
        expectedRegister = registers => registers(destParam.toRegister) = value
      )
    }
  }

  test("LD_R8_R8") {
    val value: UByte = 0x33.toUByte
    forNonMemHLR8OpCodeParamPairs { (srcParam, destParam) =>
      val opcode: UByte = OpCode.LD_R8_R8.setParam(destParam -> 3, srcParam -> 0)
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R8_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_R8_R8](opcode, instruction) { ld =>
        assertEquals(ld.src, srcParam)
        assertEquals(ld.dest, destParam)
      }

      testInstruction(
        instruction = instruction,
        setupRegister = registers => registers(srcParam.toRegister) = value,
        expectedRegister = registers => registers(destParam.toRegister) = value
      )
    }
  }
}

