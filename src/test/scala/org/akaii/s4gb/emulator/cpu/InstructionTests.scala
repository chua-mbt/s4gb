package org.akaii.s4gb.emulator.cpu

import utest.*

object InstructionTests extends TestSuite {
  val tests: Tests = Tests {
    test("NOP") {
      val instruction = Instruction.decode(0x00)
      assert(instruction == Instruction.NOP)
    }

    test("LD_R16_IMM16 - BC") {
      // Opcode: 00DDD001, DDD = 000 (BC) -> 0x01
      // imm16 = 0x1234, little-endian: 0x34 0x12
      val input = 0x01 | (0x34 << 8) | (0x12 << 16)
      val instruction = Instruction.decode(input)
      assert(instruction.isInstanceOf[Instruction.LD_R16_IMM16])
      assert(instruction.opCode == 0x01)
      assert(instruction.imm16.contains(0x1234))
    }

    test("LD_R16MEM_A - BC") {
      // Opcode: 00DDD010, DDD = 000 (BC) -> 0x02
      val input = 0x02
      val instruction = Instruction.decode(input)
      assert(instruction.isInstanceOf[Instruction.LD_R16MEM_A])
      assert(instruction.opCode == 0x02)
    }

    test("LD_A_R16MEM - BC") {
      // Opcode: 00SSS1010, SSS = 000 (BC) -> 0x0A
      val input = 0x0A
      val instruction = Instruction.decode(input)
      assert(instruction.isInstanceOf[Instruction.LD_A_R16MEM])
      assert(instruction.opCode == 0x0A)
    }

    test("LD_MEM_IMM16_SP") {
      // Opcode: 0x08, imm16 = 0x1234, little-endian: 0x34 0x12
      val input = 0x08 | (0x34 << 8) | (0x12 << 16)
      val instruction = Instruction.decode(input)
      assert(instruction.isInstanceOf[Instruction.LD_MEM_IMM16_SP])
      assert(instruction.opCode == 0x08)
      assert(instruction.imm16.contains(0x1234))
    }
  }
}
