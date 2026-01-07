package org.akaii.s4gb.emulator.cpu

import utest.*

object InstructionTests extends TestSuite {
  val tests: Tests = Tests {
    // TODO: Loop through all possible registers for each instruction that supports registers
    // TODO: Verify stringification for each instruction

    test("NOP") {
      val instruction = Instruction.decode(OpCode.NOP.pattern)
      assert(instruction == Instruction.NOP)
      assert(instruction.toString == "NOP(0x000000)")
      // TODO: verify no change in state
    }

    test("LD_R16_IMM16 - DE") {
      val input = (OpCode.LD_R16_IMM16.pattern | (Registers.R16.DE.ordinal << 4)) | (0x34 << 8) | (0x12 << 16)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.LD_R16_IMM16])
      assert(instruction.toString == "LD_R16_IMM16(0x113412)")
      assert(instruction.opCode == (OpCode.LD_R16_IMM16.pattern | (Registers.R16.DE.ordinal << 4)))
      assert(instruction.imm16.contains(0x1234))
      assert(instruction.asInstanceOf[Instruction.LD_R16_IMM16].dest == Registers.R16.DE)
      // TODO: verify the effect (actual loading)
    }

    test("LD_R16MEM_A - DE") {
      val input = OpCode.LD_R16MEM_A.pattern | (Registers.R16.DE.ordinal << 4)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.LD_R16MEM_A])
      assert(instruction.toString == "LD_R16MEM_A(0x120000)")
      assert(instruction.opCode == (OpCode.LD_R16MEM_A.pattern | (Registers.R16.DE.ordinal << 4)))
      assert(instruction.asInstanceOf[Instruction.LD_R16MEM_A].destRef == Registers.R16.DE)
      // TODO: verify the actual memory reference loaded into DE
      // TODO: verify the effect (actual loading)
    }

    test("LD_A_R16MEM - DE") {
      val input = OpCode.LD_A_R16MEM.pattern | (Registers.R16.DE.ordinal << 4)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.LD_A_R16MEM])
      assert(instruction.opCode == (OpCode.LD_A_R16MEM.pattern | (Registers.R16.DE.ordinal << 4)))
      assert(instruction.asInstanceOf[Instruction.LD_A_R16MEM].srcRef == Registers.R16.DE)
      // TODO: verify the actual memory reference loaded into DE
      // TODO: verify the effect (actual loading)
    }

    test("LD_MEM_IMM16_SP") {
      val input = OpCode.LD_MEM_IMM16_SP.pattern | (0x78 << 8) | (0x56 << 16)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.LD_MEM_IMM16_SP])
      assert(instruction.opCode == OpCode.LD_MEM_IMM16_SP.pattern)
      assert(instruction.imm16.contains(0x5678))
      // TODO: verify the effect (actual loading)
    }

    test("INC_R16 - DE") {
      val input = OpCode.INC_R16.pattern | (Registers.R16.DE.ordinal << 4)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.INC_R16])
      assert(instruction.opCode == (OpCode.INC_R16.pattern | (Registers.R16.DE.ordinal << 4)))
      assert(instruction.asInstanceOf[Instruction.INC_R16].operand == Registers.R16.DE)
      // TODO: verify the effect (increment)
    }

    test("DEC_R16 - DE") {
      val input = OpCode.DEC_R16.pattern | (Registers.R16.DE.ordinal << 4)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.DEC_R16])
      assert(instruction.opCode == (OpCode.DEC_R16.pattern | (Registers.R16.DE.ordinal << 4)))
      assert(instruction.asInstanceOf[Instruction.DEC_R16].operand == Registers.R16.DE)
      // TODO: verify the effect (decrement)
    }

    test("ADD_HL_R16 - DE") {
      val input = OpCode.ADD_HL_R16.pattern | (Registers.R16.DE.ordinal << 4)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.ADD_HL_R16])
      assert(instruction.opCode == (OpCode.ADD_HL_R16.pattern | (Registers.R16.DE.ordinal << 4)))
      assert(instruction.asInstanceOf[Instruction.ADD_HL_R16].operand == Registers.R16.DE)
      // TODO: verify the effect (HL += DE)
    }

    test("INC_R8 - C") {
      val input = OpCode.INC_R8.pattern | (Registers.R8.C.ordinal << 3)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.INC_R8])
      assert(instruction.opCode == (OpCode.INC_R8.pattern | (Registers.R8.C.ordinal << 3)))
      assert(instruction.asInstanceOf[Instruction.INC_R8].operand == Registers.R8.C)
      // TODO: verify the effect (increment)
    }

    test("DEC_R8 - C") {
      val input = OpCode.DEC_R8.pattern | (Registers.R8.C.ordinal << 3)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.DEC_R8])
      assert(instruction.opCode == (OpCode.DEC_R8.pattern | (Registers.R8.C.ordinal << 3)))
      assert(instruction.asInstanceOf[Instruction.DEC_R8].operand == Registers.R8.C)
      // TODO: verify the effect (decrement)
    }

    test("LD_R8_IMM8 - E") {
      val input = OpCode.LD_R8_IMM8.pattern | (Registers.R8.E.ordinal << 3) | (0x7F << 8)
      val instruction = Instruction.decode(input)

      assert(instruction.isInstanceOf[Instruction.LD_R8_IMM8])
      assert(instruction.opCode == (OpCode.LD_R8_IMM8.pattern | (Registers.R8.E.ordinal << 3)))
      assert(instruction.imm8.contains(0x7F))
      assert(instruction.asInstanceOf[Instruction.LD_R8_IMM8].dest == Registers.R8.E)
      // TODO: verify the effect (actual loading)
    }
  }
}
