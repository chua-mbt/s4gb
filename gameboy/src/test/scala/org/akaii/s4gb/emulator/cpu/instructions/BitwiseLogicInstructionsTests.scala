package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.{UByte, UShort}

class BitwiseLogicInstructionsTests extends InstructionsTest {
  test("CPL") {
    val instruction = Instruction.decode(Array(OpCode.CPL.pattern))
    assertEquals(instruction.toString, "CPL(0x2F)")
    verifyInstruction[Instruction.CPL.type](OpCode.CPL.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0x00.toUByte // all flags clear
      },
      expectedRegister = regs => {
        regs.a = 0xAA.toUByte // 10101010
        regs.f = 0x60.toUByte // N=1, H=1, Z & C unchanged
      }
    )

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0xAA.toUByte // 10101010
        regs.f = 0xF0.toUByte // previous flags set
      },
      expectedRegister = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0xF0.toUByte // N=1, H=1, Z & C unchanged
      }
    )
  }

  test("AND_A_MEM_HL") {
    val instruction = Instruction.decode(Array(OpCode.AND_A_MEM_HL.pattern))
    assertEquals(instruction.toString, "AND_A_MEM_HL(0xA6)")
    verifyInstruction[Instruction.AND_A_MEM_HL.type](OpCode.AND_A_MEM_HL.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.hl = 0xC000.toUShort
        regs.f = 0x00.toUByte
      },
      setupMemory = (_, memory) => {
        memory.write(0xC000.toUShort, 0xAA.toUByte) // 10101010
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=0
      }
    )

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0xFF.toUByte
        regs.hl = 0xC000.toUShort
        regs.f = 0x00.toUByte
      },
      setupMemory = (_, memory) => {
        memory.write(0xC000.toUShort, 0x0F.toUByte)
      },
      expectedRegister = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
      }
    )
  }

  test("AND_A_R8") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.AND_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"AND_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.AND_A_R8](opcode, instruction) { and =>
        assertEquals(and.operand, operandParam)
      }

      if (operandParam == OpCode.Parameters.R8.A) {
        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0x55.toUByte // 01010101
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x55.toUByte
            regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
          }
        )
      } else {
        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0x55.toUByte // 01010101
            regs(operandParam.toRegister) = 0xAA.toUByte // 10101010
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x00.toUByte
            regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=0
          }
        )

        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0xFF.toUByte
            regs(operandParam.toRegister) = 0x0F.toUByte
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x0F.toUByte
            regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
          }
        )
      }
    }
  }

  test("XOR_A_MEM_HL") {
    val instruction = Instruction.decode(Array(OpCode.XOR_A_MEM_HL.pattern))
    assertEquals(instruction.toString, "XOR_A_MEM_HL(0xAE)")
    verifyInstruction[Instruction.XOR_A_MEM_HL.type](OpCode.XOR_A_MEM_HL.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0xFF.toUByte
        regs.hl = 0xC000.toUShort
        regs.f = 0x00.toUByte
      },
      setupMemory = (_, memory) => {
        memory.write(0xC000.toUShort, 0xFF.toUByte)
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
      }
    )

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x0F.toUByte
        regs.hl = 0xC000.toUShort
        regs.f = 0x00.toUByte
      },
      setupMemory = (_, memory) => {
        memory.write(0xC000.toUShort, 0xF0.toUByte)
      },
      expectedRegister = regs => {
        regs.a = 0xFF.toUByte
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("XOR_A_R8") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.XOR_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"XOR_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.XOR_A_R8](opcode, instruction) { xor =>
        assertEquals(xor.operand, operandParam)
      }

      if (operandParam == OpCode.Parameters.R8.A) {
        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0x55.toUByte
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x00.toUByte
            regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
          }
        )
      } else {
        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0xFF.toUByte
            regs(operandParam.toRegister) = 0xFF.toUByte
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x00.toUByte
            regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
          }
        )

        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0x0F.toUByte
            regs(operandParam.toRegister) = 0xF0.toUByte
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0xFF.toUByte
            regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
          }
        )
      }
    }
  }

  test("OR_A_MEM_HL") {
    val instruction = Instruction.decode(Array(OpCode.OR_A_MEM_HL.pattern))
    assertEquals(instruction.toString, "OR_A_MEM_HL(0xB6)")
    verifyInstruction[Instruction.OR_A_MEM_HL.type](OpCode.OR_A_MEM_HL.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x00.toUByte
        regs.hl = 0xC000.toUShort
        regs.f = 0x00.toUByte
      },
      setupMemory = (_, memory) => {
        memory.write(0xC000.toUShort, 0x00.toUByte)
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=
      }
    )

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x01.toUByte
        regs.hl = 0xC000.toUShort
        regs.f = 0x00.toUByte
      },
      setupMemory = (_, memory) => {
        memory.write(0xC000.toUShort, 0x02.toUByte)
      },
      expectedRegister = regs => {
        regs.a = 0x03.toUByte
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("OR_A_R8") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.OR_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"OR_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.OR_A_R8](opcode, instruction) { xor =>
        assertEquals(xor.operand, operandParam)
      }

      if (operandParam == OpCode.Parameters.R8.A) {
        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0x55.toUByte
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x55.toUByte
            regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
          }
        )
      } else {
        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0x00.toUByte
            regs(operandParam.toRegister) = 0x00.toUByte
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x00.toUByte
            regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=
          }
        )

        testInstruction(
          instruction,
          setupRegister = regs => {
            regs.a = 0x01.toUByte
            regs(operandParam.toRegister) = 0x02.toUByte
            regs.f = 0x00.toUByte
          },
          expectedRegister = regs => {
            regs.a = 0x03.toUByte
            regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
          }
        )
      }
    }
  }

  test("AND_A_IMM8") {
    val instructionZero = Instruction.decode(Array(OpCode.AND_A_IMM8.pattern, 0xAA.toUByte))
    assertEquals(instructionZero.toString, f"AND_A_IMM8(0xE6AA)")
    verifyInstruction[Instruction.AND_A_IMM8](OpCode.AND_A_IMM8.pattern, instructionZero) { and =>
      assertEquals(and.imm8, 0xAA.toUByte)
    }

    testInstruction(
      instructionZero,
      setupRegister = regs => {
        regs.a = 0x55.toUByte // 01010101
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=0
      }
    )

    val instructionNonZero = Instruction.decode(Array(OpCode.AND_A_IMM8.pattern, 0x0F.toUByte))
    verifyInstruction[Instruction.AND_A_IMM8](OpCode.AND_A_IMM8.pattern, instructionNonZero) { and =>
      assertEquals(and.imm8, 0x0F.toUByte)
    }

    testInstruction(
      instructionNonZero,
      setupRegister = regs => {
        regs.a = 0xFF.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
      }
    )
  }

  test("XOR_A_IMM8") {
    val instructionZero = Instruction.decode(Array(OpCode.XOR_A_IMM8.pattern, 0xFF.toUByte))
    assertEquals(instructionZero.toString, f"XOR_A_IMM8(0xEEFF)")
    verifyInstruction[Instruction.XOR_A_IMM8](OpCode.XOR_A_IMM8.pattern, instructionZero) { xor =>
      assertEquals(xor.imm8, 0xFF.toUByte)
    }

    testInstruction(
      instructionZero,
      setupRegister = regs => {
        regs.a = 0xFF.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
      }
    )

    val instructionNonZero = Instruction.decode(Array(OpCode.XOR_A_IMM8.pattern, 0xF0.toUByte))
    verifyInstruction[Instruction.XOR_A_IMM8](OpCode.XOR_A_IMM8.pattern, instructionNonZero) { xor =>
      assertEquals(xor.imm8, 0xF0.toUByte)
    }

    testInstruction(
      instructionNonZero,
      setupRegister = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0xFF.toUByte
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("OR_A_IMM8") {
    val instructionZero = Instruction.decode(Array(OpCode.OR_A_IMM8.pattern, 0x00.toUByte))
    assertEquals(instructionZero.toString, f"OR_A_IMM8(0xF600)")
    verifyInstruction[Instruction.OR_A_IMM8](OpCode.OR_A_IMM8.pattern, instructionZero) { or =>
      assertEquals(or.imm8, 0x00.toUByte)
    }

    testInstruction(
      instructionZero,
      setupRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=
      }
    )

    val instructionNonZero = Instruction.decode(Array(OpCode.OR_A_IMM8.pattern, 0x02.toUByte))
    verifyInstruction[Instruction.OR_A_IMM8](OpCode.OR_A_IMM8.pattern, instructionNonZero) { or =>
      assertEquals(or.imm8, 0x02.toUByte)
    }

    testInstruction(
      instructionNonZero,
      setupRegister = regs => {
        regs.a = 0x01.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x03.toUByte
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }
}

