package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}
import utest.*

object LoadInstructionsTests extends InstructionsTest {

  val tests: Tests = Tests {
    test("LD_R16_IMM16") {
      val imm16 = 0x1234.toUShort
      Registers.R16.values.foreach { register =>
        val opcode: UByte = OpCode.LD_R16_IMM16.pattern | (register.ordinal << 4).toUByte
        val immLo: UByte = imm16.registerLoByte
        val immHi: UByte = imm16.registerHiByte

        val input: Array[UByte] = Array(opcode, immLo, immHi)
        val instruction = Instruction.decode(input)

        assert(instruction.toString == f"LD_R16_IMM16(0x${opcode.toInt}%02X${immLo.toInt}%02X${immHi.toInt}%02X)")
        verifyInstruction[Instruction.LD_R16_IMM16](opcode, instruction) { ld =>
          assert(ld.imm16 == imm16)
          assert(ld.dest == register)
        }

        val registers = Registers()
        val finalState = exhaustInstruction(instruction, Instruction.State(registers, TestMap()))

        val expectedRegisters = Registers()
        // Results
        expectedRegisters.pc = instruction.bytes.toUShort
        expectedRegisters.sp = instruction.cycles.toUShort
        expectedRegisters(register) = imm16

        verifyState(
          finalState, instruction,
          expectedRegisters = expectedRegisters,
          expectedMemory = TestMap()
        )
      }
    }

    test("LD_R16MEM_A") {
      Registers.R16.values.foreach { addressRegister =>
        val opcode: UByte = OpCode.LD_R16MEM_A.pattern | (addressRegister.ordinal << 4).toUByte
        val input: Array[UByte] = Array(opcode)
        val instruction = Instruction.decode(input)

        assert(instruction.toString == f"LD_R16MEM_A(0x${opcode.toInt}%02X)")
        verifyInstruction[Instruction.LD_R16MEM_A](opcode, instruction) { ld =>
          assert(ld.destRef == addressRegister)
        }

        val registers = Registers()
        registers.a = 0x42.toUByte
        val finalState = exhaustInstruction(instruction, Instruction.State(registers, TestMap()))

        val expectedRegisters = Registers()
        expectedRegisters.a = 0x42.toUByte
        // Results
        expectedRegisters.pc = instruction.bytes.toUShort
        expectedRegisters.sp = instruction.cycles.toUShort

        val expectedMemory = TestMap()
        // Results
        expectedMemory.write(registers(addressRegister), registers.a)

        verifyState(
          finalState, instruction,
          expectedRegisters = expectedRegisters,
          expectedMemory = expectedMemory
        )
      }
    }

    test("LD_A_R16MEM") {
      Registers.R16.values.foreach { srcRegister =>
        val opcode: UByte = OpCode.LD_A_R16MEM.pattern | (srcRegister.ordinal << 4).toUByte
        val input: Array[UByte] = Array(opcode)
        val instruction = Instruction.decode(input)

        assert(instruction.toString == f"LD_A_R16MEM(0x${opcode.toInt}%02X)")
        verifyInstruction[Instruction.LD_A_R16MEM](opcode, instruction) { ld =>
          assert(ld.srcRef == srcRegister)
        }

        val registers = Registers()
        val memory = TestMap()

        val value: UByte = 0x42.toUByte
        registers(srcRegister) = 0xC000.toUShort
        memory.write(registers(srcRegister), value)

        val finalState = exhaustInstruction(instruction, Instruction.State(registers, memory))

        val expectedRegisters = Registers()
        expectedRegisters(srcRegister) = 0xC000.toUShort

        // Results
        expectedRegisters.pc = instruction.bytes.toUShort
        expectedRegisters.sp = instruction.cycles.toUShort
        expectedRegisters.a = value

        verifyState(
          finalState, instruction,
          expectedRegisters = expectedRegisters,
          expectedMemory = memory
        )
      }
    }

    test("LD_R8_IMM8") {
      val imm8: UByte = 0x42.toUByte
      Registers.R8.values.foreach { destRegister =>
        val opcode: UByte = OpCode.LD_R8_IMM8.pattern | (destRegister.ordinal << 3).toUByte
        val input: Array[UByte] = Array(opcode, imm8)
        val instruction = Instruction.decode(input)

        assert(instruction.toString == f"LD_R8_IMM8(0x${opcode.toInt}%02X${imm8.toInt}%02X)")
        verifyInstruction[Instruction.LD_R8_IMM8](opcode, instruction) { ld =>
          assert(ld.dest == destRegister)
          assert(ld.imm8 == imm8)
        }

        val registers = Registers()
        val memory = TestMap()

        val finalState = exhaustInstruction(instruction, Instruction.State(registers, memory))
        val expectedRegisters = Registers()

        // Results
        expectedRegisters.pc = instruction.bytes.toUShort
        expectedRegisters.sp = instruction.cycles.toUShort
        expectedRegisters(destRegister) = imm8

        verifyState(
          finalState, instruction,
          expectedRegisters = expectedRegisters,
          expectedMemory = memory
        )
      }
    }

  }
}
