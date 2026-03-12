package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.memorymap.TestMap
import org.akaii.s4gb.emulator.setParam
import spire.math.UByte

class BitShiftInstructionsTests extends InstructionsTest {

  test("RLCA - C=1") {
    val opcode: UByte = OpCode.Base.RLCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RLCA(0x${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RLCA.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.a = 0x85.toUByte, // 10000101
      expectedRegister = regs => {
        regs.a = 0x0B.toUByte // 00001011
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RLCA - C=0") {
    val opcode: UByte = OpCode.Base.RLCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.a = 0x42.toUByte, // 01000010
      expectedRegister = regs => {
        regs.a = 0x84.toUByte // 10000100
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("RRCA - C=1") {
    val opcode: UByte = OpCode.Base.RRCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RRCA(0x${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RRCA.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.a = 0x01.toUByte, // 00000001
      expectedRegister = regs => {
        regs.a = 0x80.toUByte // 10000000
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RRCA - C=0") {
    val opcode: UByte = OpCode.Base.RRCA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.a = 0x84.toUByte, // 10000100
      expectedRegister = regs => {
        regs.a = 0x42.toUByte // 01000010
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("RLA - C=1") {
    val opcode: UByte = OpCode.Base.RLA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RLA(0x${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RLA.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x85.toUByte // 10000101
        regs.f = 0x10.toUByte // C=1
      },
      expectedRegister = regs => {
        regs.a = 0x0B.toUByte // 00001011
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RLA - C=0") {
    val opcode: UByte = OpCode.Base.RLA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x42.toUByte // 01000010
        regs.f = 0x00.toUByte // C=0
      },
      expectedRegister = regs => {
        regs.a = 0x84.toUByte // 10000100
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("RLA - carry in") {
    val opcode: UByte = OpCode.Base.RLA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x01.toUByte // 00000001
        regs.f = 0x10.toUByte // old C=1
      },
      expectedRegister = regs => {
        regs.a = 0x03.toUByte // 00000011 (bit 0 got old carry)
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0 (carry out from bit 7)
      }
    )
  }

  test("RRA - C=1") {
    val opcode: UByte = OpCode.Base.RRA.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, f"RRA(0x${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RRA.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x01.toUByte // 00000001
        regs.f = 0x10.toUByte // C=1
      },
      expectedRegister = regs => {
        regs.a = 0x80.toUByte // 10000000
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("RRA - C=0") {
    val opcode: UByte = OpCode.Base.RRA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x84.toUByte // 10000100
        regs.f = 0x00.toUByte // C=0
      },
      expectedRegister = regs => {
        regs.a = 0x42.toUByte // 01000010
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("RRA - carry in") {
    val opcode: UByte = OpCode.Base.RRA.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x80.toUByte // 10000000
        regs.f = 0x10.toUByte // old C=1
      },
      expectedRegister = regs => {
        regs.a = 0xC0.toUByte // 11000000 (old carry shifted into bit 7)
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0 (carry out from bit 0)
      }
    )
  }

  test("RLC_MEM_HL - C=1 Z=0") {
    val opcode: UByte = OpCode.CB.RLC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"RLC_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RLC_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x85.toUByte), // 10000101, high bit set
      expectedRegister = regs => regs.f = 0x10.toUByte, // Z=0, N=0, H=0, C=1 (carry set from bit 7)
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x0B.toUByte),
    )
  }

  test("RLC_MEM_HL - C=0 Z=1") {
    val opcode: UByte = OpCode.CB.RLC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"RLC_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RLC_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte), // 00000000
      expectedRegister = regs => regs.f = 0x80.toUByte, // Z=1, N=0, H=0, C=0
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte)
    )
  }

  test("RLC_R8 - C=1 Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RLC_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"RLC_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.RLC_R8](opcode, instruction) { rlc =>
        assertEquals(rlc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x85.toUByte, // 10000101 → carry
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x0B.toUByte // rotated left → 00001011
          regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
        }
      )
    }
  }

  test("RLC_R8 - C=0 Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RLC_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"RLC_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.RLC_R8](opcode, instruction) { rlc =>
        assertEquals(rlc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte, // zero value
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
        }
      )
    }
  }

  test("RRC_MEM_HL - C=1 Z=0") {
    val opcode: UByte = OpCode.CB.RRC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"RRC_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RRC_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x01.toUByte), // 00000001 → bit 0 = 1 → carry
      expectedRegister = regs => regs.f = 0x10.toUByte, // Z=0, C=1
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x80.toUByte) // rotated right → 10000000
    )
  }

  test("RRC_MEM_HL - C=0 Z=1") {
    val opcode: UByte = OpCode.CB.RRC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"RRC_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RRC_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte), // 00000000 → bit 0 = 0
      expectedRegister = regs => regs.f = 0x80.toUByte, // Z=1, C=0
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte) // still 0x00
    )
  }

  test("RRC_R8 - C=1 Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RRC_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"RRC_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.RRC_R8](opcode, instruction) { rrc =>
        assertEquals(rrc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x01.toUByte, // 00000001 → carry
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x80.toUByte // rotated right → 10000000
          regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
        }
      )
    }
  }

  test("RRC_R8 - C=0 Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RRC_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"RRC_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.RRC_R8](opcode, instruction) { rrc =>
        assertEquals(rrc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte, // zero value
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
        }
      )
    }
  }

  test("RL_MEM_HL - C=1 Z=0") {
    val opcode: UByte = OpCode.CB.RL_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"RL_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RL_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x85.toUByte), // 10000101
      expectedRegister = regs => regs.f = 0x10.toUByte, // Z=0, N=0, H=0, C=1 (carry out from bit 7)
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x0A.toUByte), // 00001010
    )
  }

  test("RL_MEM_HL - C=0 Z=1") {
    val opcode: UByte = OpCode.CB.RL_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0x80.toUByte, // Z=1, C=0
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte)
    )
  }

  test("RL_MEM_HL - carry-in") {
    val opcode: UByte = OpCode.CB.RL_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.hl = 0xC000.toUShort
        regs.f = 0x10.toUByte // C=1
      },
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x40.toUByte), // 01000000
      expectedRegister = regs => regs.f = 0x00.toUByte, // C=0, Z=0
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x81.toUByte) // 10000001 (carry-in applied)
    )
  }

  test("RL_R8 - C=1 Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RL_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"RL_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.RL_R8](opcode, instruction) { rl =>
        assertEquals(rl.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs(operandParam.toRegister) = 0x85.toUByte // 10000101
          regs.f = 0x10.toUByte // C=1
        },
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x0B.toUByte // 00001011, carry in becomes bit 0
          regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1 (carry out from bit 7)
        }
      )
    }
  }

  test("RL_R8 - C=0 Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RL_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte // zero value
          regs.f = 0x00.toUByte // C=0
        },
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
        }
      )
    }
  }

  test("RL_R8 - carry-in") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RL_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs(operandParam.toRegister) = 0x40.toUByte // 01000000
          regs.f = 0x10.toUByte // C=1
        },
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x81.toUByte // 10000001 (carry-in applied)
          regs.f = 0x00.toUByte // C=0, Z=0
        }
      )
    }
  }

  test("RR_MEM_HL - C=1 Z=0") {
    val opcode: UByte = OpCode.CB.RR_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"RR_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.RR_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.hl = 0xC000.toUShort
        regs.f = 0x10.toUByte // C=1
      },
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x01.toUByte), // 00000001
      expectedRegister = regs => regs.f = 0x10.toUByte, // Z=0, N=0, H=0, C=1 (carry out from bit 0)
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x80.toUByte), // 10000000, carry in becomes bit 7
    )
  }

  test("RR_MEM_HL - C=0 Z=1") {
    val opcode: UByte = OpCode.CB.RR_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0x80.toUByte, // Z=1, C=0
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte)
    )
  }

  test("RR_MEM_HL - carry-in") {
    val opcode: UByte = OpCode.CB.RR_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.hl = 0xC000.toUShort
        regs.f = 0x10.toUByte // C=1
      },
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x02.toUByte), // 00000010
      expectedRegister = regs => regs.f = 0x00.toUByte, // C=0 (original bit 0)
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x81.toUByte) // 10000001 (carry-in applied)
    )
  }

  test("RR_R8 - C=1 Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RR_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"RR_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.RR_R8](opcode, instruction) { rr =>
        assertEquals(rr.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs(operandParam.toRegister) = 0x01.toUByte // 00000001
          regs.f = 0x10.toUByte // C=1
        },
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x80.toUByte // 10000000, carry in becomes bit 7
          regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1 (carry out from bit 0)
        }
      )
    }
  }

  test("RR_R8 - C=0 Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.RR_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
        }
      )
    }
  }

  test("RR_MEM_HL - carry-in") {
    val opcode: UByte = OpCode.CB.RR_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.hl = 0xC000.toUShort
        regs.f = 0x10.toUByte // C=1
      },
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x02.toUByte), // 00000010
      expectedRegister = regs => regs.f = 0x00.toUByte, // C=0 (original bit 0)
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x81.toUByte) // 10000001 (carry-in applied)
    )
  }

  test("SLA_MEM_HL - C=1 Z=0") {
    val opcode: UByte = OpCode.CB.SLA_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"SLA_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.SLA_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0xC0.toUByte), // 11000000
      expectedRegister = regs => regs.f = 0x10.toUByte, // Z=0, C=1
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x80.toUByte) // 10000000
    )
  }

  test("SLA_MEM_HL - C=0 Z=1") {
    val opcode: UByte = OpCode.CB.SLA_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0x80.toUByte, // Z=1, C=0
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte)
    )
  }

  test("SLA_R8 - C=1 Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SLA_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"SLA_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SLA_R8](opcode, instruction) { shift =>
        assertEquals(shift.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0xC0.toUByte, // 11000000
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x80.toUByte // 10000000
          regs.f = 0x10.toUByte // Z=0, C=1
        }
      )
    }
  }

  test("SLA_R8 - C=0 Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SLA_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte // Z=1, C=0
        }
      )
    }
  }

  test("SRA_MEM_HL - C=1 Z=0") {
    val opcode: UByte = OpCode.CB.SRA_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"SRA_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.SRA_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x03.toUByte), // 00000011
      expectedRegister = regs => regs.f = 0x10.toUByte, // Z=0, C=1
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x01.toUByte) // 00000001
    )
  }

  test("SRA_MEM_HL - C=0 Z=1") {
    val opcode: UByte = OpCode.CB.SRA_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0x80.toUByte, // Z=1, C=0
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte)
    )
  }

  test("SRA_R8 - C=1 Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SRA_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"SRA_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SRA_R8](opcode, instruction) { shift =>
        assertEquals(shift.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x03.toUByte, // 00000011
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x01.toUByte // 00000001
          regs.f = 0x10.toUByte // Z=0, C=1
        }
      )
    }
  }

  test("SRA_R8 - C=0 Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SRA_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte // Z=1, C=0
        }
      )
    }
  }

  test("SWAP_MEM_HL - Z=1") {
    val opcode: UByte = OpCode.CB.SWAP_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"SWAP_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.SWAP_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0xA5.toUByte),
      expectedRegister = regs => regs.f = 0x00.toUByte,
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x5A.toUByte)
    )
  }

  test("SWAP_MEM_HL - Z=0") {
    val opcode: UByte = OpCode.CB.SWAP_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0x80.toUByte,
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte)
    )
  }

  test("SWAP_R8 - Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SWAP_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"SWAP_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SWAP_R8](opcode, instruction) { swap =>
        assertEquals(swap.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0xA5.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x5A.toUByte
          regs.f = 0x00.toUByte
        }
      )
    }
  }

  test("SWAP_R8 - Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SWAP_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte
        }
      )
    }
  }

  test("SRL_MEM_HL - C=1 Z=0") {
    val opcode: UByte = OpCode.CB.SRL_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    assertEquals(instruction.toString, f"SRL_MEM_HL(0xCB${opcode.toInt}%02X)")
    verifyInstruction[Instruction.SRL_MEM_HL](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x03.toUByte),
      expectedRegister = regs => regs.f = 0x10.toUByte,
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x01.toUByte)
    )
  }

  test("SRL_MEM_HL - C=0 Z=1") {
    val opcode: UByte = OpCode.CB.SRL_MEM_HL.pattern
    val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, mem) => mem.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0x80.toUByte,
      expectedMemory = mem => mem.write(0xC000.toUShort, 0x00.toUByte)
    )
  }

  test("SRL_R8 - C=1 Z=0") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SRL_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      assertEquals(instruction.toString, f"SRL_R8(0xCB${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SRL_R8](opcode, instruction) { shift =>
        assertEquals(shift.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x03.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x01.toUByte
          regs.f = 0x10.toUByte
        }
      )
    }
  }

  test("SRL_R8 - C=0 Z=1") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CB.SRL_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(0xCB.toUByte, opcode))

      testInstruction(
        instruction,
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0x80.toUByte
        }
      )
    }
  }
}
