package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class LoadInstructionsTests extends InstructionsTest {
  test("LD_R16_IMM16") {
    val imm16 = 0x1234.toUShort
    forAllR16 { register =>
      val opcode: UByte = OpCode.LD_R16_IMM16.pattern | (register.ordinal << 4).toUByte
      val immLo: UByte = imm16.registerLoByte
      val immHi: UByte = imm16.registerHiByte

      val input: Array[UByte] = Array(opcode, immLo, immHi)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R16_IMM16(0x${opcode.toInt}%02X${immLo.toInt}%02X${immHi.toInt}%02X)")
      verifyInstruction[Instruction.LD_R16_IMM16](opcode, instruction) { ld =>
        assertEquals(ld.imm16, imm16)
        assertEquals(ld.dest, register)
      }

      testInstruction(
        instruction = instruction,
        registerExpect = registers => registers(register) = imm16
      )
    }
  }

  test("LD_R16MEM_A") {
    forAllR16 { addressRegister =>
      val opcode: UByte = OpCode.LD_R16MEM_A.pattern | (addressRegister.ordinal << 4).toUByte
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R16MEM_A(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_R16MEM_A](opcode, instruction) { ld =>
        assertEquals(ld.destRef, addressRegister)
      }

      testInstruction(
        instruction = instruction,
        registerSetup = registers => {
          registers.a = 0x42.toUByte
          registers(addressRegister) = 0xC000.toUShort
        },
        memoryExpect = memory => {
          memory.write(0xC000.toUShort, 0x42.toUByte)
        }
      )
    }
  }

  test("LD_A_R16MEM") {
    forAllR16 { srcRegister =>
      val opcode: UByte = OpCode.LD_A_R16MEM.pattern | (srcRegister.ordinal << 4).toUByte
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_A_R16MEM(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_A_R16MEM](opcode, instruction) { ld =>
        assertEquals(ld.srcRef, srcRegister)
      }

      testInstruction(
        instruction = instruction,
        registerSetup = registers => registers(srcRegister) = 0xC000.toUShort,
        memorySetup = (registers, memory) => memory.write(registers(srcRegister), 0x42.toUByte),
        registerExpect = _.a = 0x42.toUByte
      )
    }
  }

  test("LD_R8_IMM8") {
    val imm8: UByte = 0x42.toUByte
    forAllR8 { destRegister =>
      val opcode: UByte = OpCode.LD_R8_IMM8.pattern | (destRegister.ordinal << 3).toUByte
      val input: Array[UByte] = Array(opcode, imm8)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R8_IMM8(0x${opcode.toInt}%02X${imm8.toInt}%02X)")
      verifyInstruction[Instruction.LD_R8_IMM8](opcode, instruction) { ld =>
        assertEquals(ld.dest, destRegister)
        assertEquals(ld.imm8, imm8)
      }

      testInstruction(
        instruction = instruction,
        registerExpect = registers => registers(destRegister) = imm8
      )
    }
  }

  test("LD_R8_R8") {
    val value: UByte = 0x33.toUByte
    forAllR8Pairs { (sourceRegister, destRegister) =>
      val opcode: UByte = OpCode.LD_R8_R8.pattern | (destRegister.ordinal << 3).toUByte | (sourceRegister.ordinal << 0).toUByte
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"LD_R8_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.LD_R8_R8](opcode, instruction) { ld =>
        assertEquals(ld.source, sourceRegister)
        assertEquals(ld.dest, destRegister)
      }

      testInstruction(
        instruction = instruction,
        registerSetup = registers => registers(sourceRegister) = value,
        registerExpect = registers => registers(destRegister) = value
      )
    }
  }
}

