package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}
import utest.*

object LoadInstructionsTests extends TestSuite {

  val tests: Tests = Tests {
    test("LD_R16_IMM16") {
      val imm16 = 0x1234.toUShort
      Registers.R16.values.foreach { register =>
        val opcode: UByte = OpCode.LD_R16_IMM16.pattern | (register.ordinal << 4).toUByte
        val immLo: UByte = imm16.registerLoByte
        val immHi: UByte = imm16.registerHiByte

        val input: Array[UByte] = Array(opcode, immLo, immHi)
        val instruction = Instruction.decode(input)

        assert(instruction.isInstanceOf[Instruction.LD_R16_IMM16])
        assert(instruction.toString == f"LD_R16_IMM16(0x${opcode.toInt}%02X${immLo.toInt}%02X${immHi.toInt}%02X)")
        assert(instruction.opCode == opcode)
        assert(instruction.asInstanceOf[Instruction.LD_R16_IMM16].imm16 == imm16)
        assert(instruction.asInstanceOf[Instruction.LD_R16_IMM16].dest == register)

        val registers = Registers()
        instruction.execute(registers, TestMap())

        assert(registers(register) == imm16)
      }
    }

    test("LD_R16MEM_A") {
      Registers.R16.values.foreach { addressRegister =>
        val opcode: UByte = OpCode.LD_R16MEM_A.pattern | (addressRegister.ordinal << 4).toUByte
        val input: Array[UByte] = Array(opcode)
        val instruction = Instruction.decode(input)

        assert(instruction.isInstanceOf[Instruction.LD_R16MEM_A])
        assert(instruction.toString == f"LD_R16MEM_A(0x${opcode.toInt}%02X)")
        assert(instruction.opCode == opcode)
        assert(instruction.asInstanceOf[Instruction.LD_R16MEM_A].destRef == addressRegister)

        val registers = Registers()
        val memory = TestMap()
        registers.a = 0x42.toUByte

        instruction.execute(registers, memory)

        val memoryAddress = registers(addressRegister)
        assert(memory(memoryAddress) == registers.a)
      }
    }

    test("LD_A_R16MEM") {
      Registers.R16.values.foreach { srcRegister =>
        val opcode: UByte = OpCode.LD_A_R16MEM.pattern | (srcRegister.ordinal << 4).toUByte
        val input: Array[UByte] = Array(opcode)
        val instruction = Instruction.decode(input)

        assert(instruction.isInstanceOf[Instruction.LD_A_R16MEM])
        assert(instruction.toString == f"LD_A_R16MEM(0x${opcode.toInt}%02X)")
        assert(instruction.opCode == opcode)
        assert(instruction.asInstanceOf[Instruction.LD_A_R16MEM].srcRef == srcRegister)

        val registers = Registers()
        val memory = TestMap()

        val value: UByte = 0x42.toUByte
        registers(srcRegister) = 0xC000.toUShort
        memory.write(registers(srcRegister), value)

        instruction.execute(registers, memory)
        assert(registers.a == value)
      }
    }

    test("LD_R8_IMM8") {
      val imm8: UByte = 0x42.toUByte
      Registers.R8.values.foreach { destRegister =>
        val opcode: UByte = OpCode.LD_R8_IMM8.pattern | (destRegister.ordinal << 3).toUByte
        val input: Array[UByte] = Array(opcode, imm8)
        val instruction = Instruction.decode(input)

        assert(instruction.isInstanceOf[Instruction.LD_R8_IMM8])
        assert(instruction.toString == f"LD_R8_IMM8(0x${opcode.toInt}%02X${imm8.toInt}%02X)")
        assert(instruction.opCode == opcode)
        assert(instruction.asInstanceOf[Instruction.LD_R8_IMM8].dest == destRegister)
        assert(instruction.asInstanceOf[Instruction.LD_R8_IMM8].imm8 == imm8)

        val registers = Registers()
        val memory = TestMap()

        instruction.execute(registers, memory)
        assert(registers(destRegister) == imm8)
      }
    }

    test("LD_R8_R8") {
      val registerPairs = Registers.R8.values.toSeq.flatMap(dest =>
        Registers.R8.values.toSeq.map(src => (dest, src))
      )

      registerPairs.foreach { case (destRegister, srcRegister) =>
        val opcode: UByte = OpCode.LD_R8_R8.pattern | (destRegister.ordinal << 3).toUByte | srcRegister.ordinal.toUByte
        val input: Array[UByte] = Array(opcode)
        val instruction = Instruction.decode(input)

        assert(instruction.isInstanceOf[Instruction.LD_R8_R8])
        assert(instruction.toString == f"LD_R8_R8(0x${opcode.toInt}%02X)")
        assert(instruction.opCode == opcode)
        assert(instruction.asInstanceOf[Instruction.LD_R8_R8].dest == destRegister)
        assert(instruction.asInstanceOf[Instruction.LD_R8_R8].source == srcRegister)

        val registers = Registers()
        val memory = TestMap()

        val value: UByte = 0x42.toUByte
        registers(srcRegister) = value

        instruction.execute(registers, memory)
        assert(registers(destRegister) == value)
      }
    }
  }
}
