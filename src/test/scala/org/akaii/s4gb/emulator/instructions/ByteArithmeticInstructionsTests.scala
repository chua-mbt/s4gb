package org.akaii.s4gb.emulator.instructions

import munit.*
import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.instructions.OpCode.Extract.range
import spire.math.{UByte, UShort}

class ByteArithmeticInstructionsTests extends InstructionsTest {
  test("INC_R8".ignore) {
    forAllR8 { register =>
      val opcode: UByte = OpCode.INC_R8.pattern | (register.ordinal << 3).toUByte
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"INC_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.INC_R8](opcode, instruction) { inc =>
        assertEquals(inc.operand, register)
      }
    }
  }

  test("INC_R8 increments value and sets flags correctly".ignore) {
    val register = Registers.R8.C
    val opcode: UByte = OpCode.INC_R8.pattern | (register.ordinal << 3).toUByte
    val input: Array[UByte] = Array(opcode)
    val instruction = Instruction.decode(input)

    println(s"Test instruction: $instruction")
    println(s"Test micro sequence: ${instruction.micro.length}")
    println(s"Test micro(0).cycles: ${instruction.micro(0).cycles}")

    val initialValue: UByte = 0x42.toUByte
    val expectedValue: UByte = 0x43.toUByte

    testInstruction(
      instruction = instruction,
      registerSetup = registers => registers(register) = initialValue,
      registerExpect = registers => {
        println(s"Expected check: $register = ${registers(register)}, should be $expectedValue")
        assertEquals(registers(register), expectedValue)
        assert(!registers.flags.z)
        assert(!registers.flags.n)
        assert(!registers.flags.h)
      }
    )
  }

  test("INC_R8 sets Z flag when result is zero".ignore) {
    val register = Registers.R8.C
    val opcode: UByte = OpCode.INC_R8.pattern | (register.ordinal << 3).toUByte
    val input: Array[UByte] = Array(opcode)
    val instruction = Instruction.decode(input)

    testInstruction(
      instruction = instruction,
      registerSetup = registers => registers(register) = 0xFF.toUByte,
      registerExpect = registers => {
        assertEquals(registers(register), 0.toUByte)
        assert(registers.flags.z)
        assert(!registers.flags.n)
      }
    )
  }

  test("INC_R8 sets H flag when bottom nibble overflows".ignore) {
    val register = Registers.R8.C
    val opcode: UByte = OpCode.INC_R8.pattern | (register.ordinal << 3).toUByte
    val input: Array[UByte] = Array(opcode)
    val instruction = Instruction.decode(input)

    testInstruction(
      instruction = instruction,
      registerSetup = registers => registers(register) = 0x0F.toUByte,
      registerExpect = registers => {
        assertEquals(registers(register), 0x10.toUByte)
        assert(registers.flags.h)
        assert(!registers.flags.n)
      }
    )
  }

  test("DEC_R8".ignore) {
    forAllR8 { register =>
      val opcode: UByte = OpCode.DEC_R8.pattern | (register.ordinal << 3).toUByte
      val input: Array[UByte] = Array(opcode)
      val instruction = Instruction.decode(input)

      assertEquals(instruction.toString, f"DEC_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.DEC_R8](opcode, instruction) { dec =>
        assertEquals(dec.operand, register)
      }
    }
  }

  test("DEC_R8 decrements value and sets flags correctly".ignore) {
    val register = Registers.R8.C
    val opcode: UByte = OpCode.DEC_R8.pattern | (register.ordinal << 3).toUByte
    val input: Array[UByte] = Array(opcode)
    val instruction = Instruction.decode(input)

    val initialValue: UByte = 0x42.toUByte
    val expectedValue: UByte = 0x41.toUByte

    testInstruction(
      instruction = instruction,
      registerSetup = registers => registers(register) = initialValue,
      registerExpect = registers => {
        assertEquals(registers(register), expectedValue)
        assert(!registers.flags.z)
        assert(registers.flags.n)
        assert(!registers.flags.h)
      }
    )
  }

  test("DEC_R8 sets Z flag when result is zero".ignore) {
    val register = Registers.R8.C
    val opcode: UByte = OpCode.DEC_R8.pattern | (register.ordinal << 3).toUByte
    val input: Array[UByte] = Array(opcode)
    val instruction = Instruction.decode(input)

    testInstruction(
      instruction = instruction,
      registerSetup = registers => registers(register) = 0x01.toUByte,
      registerExpect = registers => {
        assertEquals(registers(register), 0.toUByte)
        assert(registers.flags.z)
        assert(registers.flags.n)
      }
    )
  }

  test("DEC_R8 sets H flag when bottom nibble underflows".ignore) {
    val register = Registers.R8.C
    val opcode: UByte = OpCode.DEC_R8.pattern | (register.ordinal << 3).toUByte
    val input: Array[UByte] = Array(opcode)
    val instruction = Instruction.decode(input)

    testInstruction(
      instruction = instruction,
      registerSetup = registers => registers(register) = 0x00.toUByte,
      registerExpect = registers => {
        assertEquals(registers(register), 0xFF.toUByte)
        assert(registers.flags.h)
        assert(registers.flags.n)
      }
    )
  }
}
