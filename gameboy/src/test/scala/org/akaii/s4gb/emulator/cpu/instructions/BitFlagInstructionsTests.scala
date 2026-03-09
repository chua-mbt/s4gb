package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.{UByte, UShort}

class BitFlagInstructionsTests extends InstructionsTest {
  test("BIT_B3_MEM_HL") {
    val initialHL: UShort = 0xC000.toUShort

    (1 to 7).foreach { bitIndex =>
      val opcode: UByte = (0x46 | (bitIndex << 3)).toUByte
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"BIT_B3_MEM_HL(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.BIT_B3_MEM_HL](opcode, instruction)

      testInstruction(
        instruction,
        setupRegister = regs => regs.hl = initialHL,
        setupMemory = (_, mem) => mem.write(initialHL, (1 << bitIndex).toUByte),
        expectedRegister = regs => regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
      )

      testInstruction(
        instruction,
        setupRegister = regs => regs.hl = initialHL,
        setupMemory = (_, mem) => mem.write(initialHL, 0x00.toUByte),
        expectedRegister = regs => regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=0
      )
    }
  }

  test("BIT_B3_R8") {
    forNonMemHLR8OpCodeParams { operandParam =>
      (1 to 7).foreach { bitIndex =>
        val opcode: UByte = OpCode.CB.BIT_B3_R8.pattern | (bitIndex << 3).toUByte | operandParam.ordinal.toUByte
        val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

        assertEquals(instruction.toString, f"BIT_B3_R8(0xCB${opcode.toInt}%02X)")
        verifyInstruction[Instruction.BIT_B3_R8](opcode, instruction) { bit =>
          assertEquals(bit.operand, operandParam)
        }

        testInstruction(
          instruction,
          setupRegister = regs => regs(operandParam.toRegister) = (1 << bitIndex).toUByte,
          expectedRegister = regs => regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
        )

        testInstruction(
          instruction,
          setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
          expectedRegister = regs => regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=0
        )
      }
    }
  }

  test("RES_B3_MEM_HL") {
    val initialHL: UShort = 0xC000.toUShort

    (1 to 7).foreach { bitIndex =>
      val opcode: UByte = (0x86 | (bitIndex << 3)).toUByte
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"RES_B3_MEM_HL(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.RES_B3_MEM_HL](opcode, instruction)

      testInstruction(
        instruction,
        setupRegister = regs => regs.hl = initialHL,
        setupMemory = (_, mem) => mem.write(initialHL, (1 << bitIndex).toUByte),
        expectedMemory = mem => mem.write(initialHL, 0x00.toUByte)
      )

      testInstruction(
        instruction,
        setupRegister = regs => regs.hl = initialHL,
        setupMemory = (_, mem) => mem.write(initialHL, 0xFF.toUByte),
        expectedMemory = mem => mem.write(initialHL, (0xFF & ~(1 << bitIndex)).toUByte)
      )
    }
  }

  test("RES_B3_R8") {
    forNonMemHLR8OpCodeParams { operandParam =>
      (1 to 7).foreach { bitIndex =>
        val opcode: UByte = OpCode.CB.RES_B3_R8.pattern | (bitIndex << 3).toUByte | operandParam.ordinal.toUByte
        val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

        assertEquals(instruction.toString, f"RES_B3_R8(0xCB${opcode.toInt}%02X)")
        verifyInstruction[Instruction.RES_B3_R8](opcode, instruction) { bit =>
          assertEquals(bit.operand, operandParam)
        }

        testInstruction(
          instruction,
          setupRegister = regs => regs(operandParam.toRegister) = (1 << bitIndex).toUByte,
          expectedRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte
        )

        testInstruction(
          instruction,
          setupRegister = regs => regs(operandParam.toRegister) = 0xFF.toUByte,
          expectedRegister = regs => regs(operandParam.toRegister) = (0xFF & ~(1 << bitIndex)).toUByte
        )
      }
    }
  }

  test("SET_B3_MEM_HL") {
    val initialHL: UShort = 0xC000.toUShort

    (1 to 7).foreach { bitIndex =>
      val opcode: UByte = (0xC6 | (bitIndex << 3)).toUByte
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"SET_B3_MEM_HL(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SET_B3_MEM_HL](opcode, instruction)

      testInstruction(
        instruction,
        setupRegister = regs => regs.hl = initialHL,
        setupMemory = (_, mem) => mem.write(initialHL, 0x00.toUByte),
        expectedMemory = mem => mem.write(initialHL, (1 << bitIndex).toUByte)
      )

      testInstruction(
        instruction,
        setupRegister = regs => regs.hl = initialHL,
        setupMemory = (_, mem) => mem.write(initialHL, 0xFF.toUByte),
        expectedMemory = mem => mem.write(initialHL, 0xFF.toUByte)
      )
    }
  }

  test("SET_B3_R8") {
    forNonMemHLR8OpCodeParams { operandParam =>
      (1 to 7).foreach { bitIndex =>
        val opcode: UByte = OpCode.CB.SET_B3_R8.pattern | (bitIndex << 3).toUByte | operandParam.ordinal.toUByte
        val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

        assertEquals(instruction.toString, f"SET_B3_R8(0xCB${opcode.toInt}%02X)")
        verifyInstruction[Instruction.SET_B3_R8](opcode, instruction) { bit =>
          assertEquals(bit.operand, operandParam)
        }

        testInstruction(
          instruction,
          setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
          expectedRegister = regs => regs(operandParam.toRegister) = (1 << bitIndex).toUByte
        )

        testInstruction(
          instruction,
          setupRegister = regs => regs(operandParam.toRegister) = 0xFF.toUByte,
          expectedRegister = regs => regs(operandParam.toRegister) = 0xFF.toUByte
        )
      }
    }
  }
}
