package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.{TestMap, setParam}
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.UByte

class BitShiftInstructionsTests extends InstructionsTest {

  test("RLCA - carry") {
    val opcode: UByte = OpCode.RLCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RLCA(0x${opcode.toInt}%02X)")
    verifyInstructionOpCode[Instruction.RLCA](opcode, instruction)

    testInstruction(
      instruction,
      registerSetup = regs => regs.a = 0x85.toUByte, // 10000101
      registerExpect = regs => {
        regs.a = 0x0B.toUByte // 00001011
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RLCA - no carry") {
    val opcode: UByte = OpCode.RLCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      registerSetup = regs => regs.a = 0x42.toUByte, // 01000010
      registerExpect = regs => {
        regs.a = 0x84.toUByte // 10000100
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("RRCA - carry") {
    val opcode: UByte = OpCode.RRCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RRCA(0x${opcode.toInt}%02X)")
    verifyInstructionOpCode[Instruction.RRCA](opcode, instruction)

    testInstruction(
      instruction,
      registerSetup = regs => regs.a = 0x01.toUByte, // 00000001
      registerExpect = regs => {
        regs.a = 0x80.toUByte // 10000000
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RRCA - no carry") {
    val opcode: UByte = OpCode.RRCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      registerSetup = regs => regs.a = 0x84.toUByte, // 10000100
      registerExpect = regs => {
        regs.a = 0x42.toUByte // 01000010
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("RLA - carry") {
    val opcode: UByte = OpCode.RLA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RLA(0x${opcode.toInt}%02X)")
    verifyInstructionOpCode[Instruction.RLA](opcode, instruction)

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x85.toUByte // 10000101
        regs.f = 0x10.toUByte // C=1
      },
      registerExpect = regs => {
        regs.a = 0x0B.toUByte // 00001011
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RLA - no carry") {
    val opcode: UByte = OpCode.RLA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x42.toUByte // 01000010
        regs.f = 0x00.toUByte // C=0
      },
      registerExpect = regs => {
        regs.a = 0x84.toUByte // 10000100
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("RRA - carry") {
    val opcode: UByte = OpCode.RRA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RRA(0x${opcode.toInt}%02X)")
    verifyInstructionOpCode[Instruction.RRA](opcode, instruction)

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x01.toUByte // 00000001
        regs.f = 0x10.toUByte // C=1
      },
      registerExpect = regs => {
        regs.a = 0x80.toUByte // 10000000
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RRA - no carry") {
    val opcode: UByte = OpCode.RRA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x84.toUByte // 10000100
        regs.f = 0x00.toUByte // C=0
      },
      registerExpect = regs => {
        regs.a = 0x42.toUByte // 01000010
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }
}
