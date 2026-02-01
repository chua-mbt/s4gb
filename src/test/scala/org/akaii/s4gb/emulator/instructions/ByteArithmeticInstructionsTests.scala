package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.UByte

class ByteArithmeticInstructionsTests extends InstructionsTest {

  test("INC_R8 - normal increment") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"INC_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.INC_R8](opcode, instruction) { inc =>
        assertEquals(inc.operand, operandParam)
      }

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x12.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x13.toUByte
          regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=unchanged
        }
      )
    }
  }

  test("INC_R8 - overflow and zero flag set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0xFF.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=unchanged
        }
      )
    }
  }

  test("INC_R8 - half-carry set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.INC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x0F.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x10.toUByte
          regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - normal decrement") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"DEC_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.DEC_R8](opcode, instruction) { dec =>
        assertEquals(dec.operand, operandParam)
      }

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x12.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x11.toUByte
          regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - zero flag set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x01.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x00.toUByte
          regs.f = 0xC0.toUByte // Z=1, N=1, H=1, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - underflow") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0xFF.toUByte
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=unchanged
        }
      )
    }
  }

  test("DEC_R8 - half-borrow set") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.DEC_R8.setParam(operandParam -> 3)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        registerSetup = regs => regs(operandParam.toRegister) = 0x10.toUByte,
        registerExpect = regs => {
          regs(operandParam.toRegister) = 0x0F.toUByte
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=unchanged
        }
      )
    }
  }

  test("ADD_A_IMM8 - normal addition") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x23.toUByte))
    verifyInstruction[Instruction.ADD_A_IMM8](OpCode.ADD_A_IMM8.pattern, instruction) { add =>
      assertEquals(add.imm8, 0x23.toUByte)
    }

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x12.toUByte
        regs.f = 0xF0.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x35.toUByte // = 0x35
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("ADD_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x00.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x70.toUByte // N=1, H=1, C=1, Z=0;
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte // 0 + 0 = 0
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
      }
    )
  }

  test("ADD_A_IMM8 - overflow/carry") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x20.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0xF0.toUByte
        regs.f = 0x00.toUByte // clear all flags initially
      },
      registerExpect = regs => {
        regs.a = 0x10.toUByte // 0xF0 + 0x20 wraps to 0x10
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("ADD_A_IMM8 - half-carry only") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x0F.toUByte // 00001111
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      },
      registerExpect = regs => {
        regs.a = 0x10.toUByte // 0x0F + 0x01 = 0x10
        regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
      }
    )
  }

  test("ADC_A_IMM8 - normal addition, carry clear") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x23.toUByte))
    verifyInstruction[Instruction.ADC_A_IMM8](OpCode.ADC_A_IMM8.pattern, instruction) { adc =>
      assertEquals(adc.imm8, 0x23.toUByte)
    }

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x12.toUByte
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      },
      registerExpect = regs => {
        regs.a = 0x35.toUByte // 0x12 + 0x23 + 0
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("ADC_A_IMM8 - addition with carry-in") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x23.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x12.toUByte
        regs.f = 0x10.toUByte // C=1, other flags cleared
      },
      registerExpect = regs => {
        regs.a = 0x36.toUByte // 0x12 + 0x23 + 1
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("ADC_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x00.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x00.toUByte // all flags cleared
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0; lower nibble cleared
      }
    )
  }

  test("ADC_A_IMM8 - overflow/carry") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x90.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x80.toUByte
        regs.f = 0x00.toUByte // all flags cleared
      },
      registerExpect = regs => {
        regs.a = 0x10.toUByte // low 8 bits of 0x110
        regs.f = 0x10.toUByte // C=1, N=0, H=0, Z=0; lower nibble zero
      }
    )
  }

  test("ADC_A_IMM8 - overflow with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x7F.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x80.toUByte
        regs.f = 0x10.toUByte // C=1 initially, lower nibble cleared
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte // 0x80 + 0x7F + 1 = 0x100 → low 8 bits = 0
        regs.f = 0xB0.toUByte // Z=1, N=0, H=1, C=1; lower nibble zero
      }
    )
  }

  test("ADC_A_IMM8 - half-carry only with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x10.toUByte // C=1 initially, lower nibble zero
      },
      registerExpect = regs => {
        regs.a = 0x11.toUByte // 0x0F + 0x01 + 1 = 0x11
        regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0; lower nibble zero
      }
    )
  }

  test("SUB_IMM8 - normal subtraction") {
    val instruction = Instruction.decode(Array(OpCode.SUB_IMM8.pattern, 0x12.toUByte))
    verifyInstruction[Instruction.SUB_IMM8](OpCode.SUB_IMM8.pattern, instruction) { sub =>
      assertEquals(sub.imm8, 0x12.toUByte)
    }

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0xF0.toUByte // all flags cleared, lower nibble ignored
      },
      registerExpect = regs => {
        regs.a = 0x11.toUByte // 0x23 - 0x12 = 0x11
        regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=0
      }
    )
  }

  test("SUB_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.SUB_IMM8.pattern, 0x23.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
      }
    )
  }

  test("SUB_IMM8 - borrow/carry") {
    val instruction = Instruction.decode(Array(OpCode.SUB_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0xC0.toUByte // 0x10 - 0x50 wraps to 0xC0
        regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1
      }
    )
  }

  test("SUB_IMM8 - half-borrow only") {
    val instruction = Instruction.decode(Array(OpCode.SUB_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
      }
    )
  }

  test("SBC_A_IMM8 - normal subtraction with carry clear") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x12.toUByte))
    verifyInstruction[Instruction.SBC_A_IMM8](OpCode.SBC_A_IMM8.pattern, instruction) { sbc =>
      assertEquals(sbc.imm8, 0x12.toUByte)
    }

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x00.toUByte // C=0
      },
      registerExpect = regs => {
        regs.a = 0x11.toUByte // 0x23 - 0x12 - 0 = 0x11
        regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=0
      }
    )
  }

  test("SBC_A_IMM8 - subtraction with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x12.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x10.toUByte // C=1 initially
      },
      registerExpect = regs => {
        regs.a = 0x10.toUByte // 0x23 - 0x12 - 1 = 0x10
        regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=0
      }
    )
  }

  test("SBC_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x22.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x10.toUByte // C=1
      },
      registerExpect = regs => {
        regs.a = 0x00.toUByte // 0x23 - 0x22 - 1 = 0
        regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
      }
    )
  }

  test("SBC_A_IMM8 - borrow/carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte // C=0 initially
      },
      registerExpect = regs => {
        regs.a = 0xC0.toUByte // 0x10 - 0x50 = -0x40 → 0xC0
        regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1; lower nibble zero
      }
    )
  }

  test("SBC_A_IMM8 - borrow with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x10.toUByte // C=1 initially
      },
      registerExpect = regs => {
        regs.a = 0xBF.toUByte // 0x10 - 0x50 - 1 = -0x41 → 0xBF
        regs.f = 0x70.toUByte // Z=0, N=1, H=0, C=1; lower nibble zero
      }
    )
  }

  test("SBC_A_IMM8 - half-borrow with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x10.toUByte // C=1 initially
      },
      registerExpect = regs => {
        regs.a = 0x0E.toUByte // 0x10 - 0x01 - 1 = 0x0E
        regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
      }
    )
  }

  test("CP_IMM8 - normal compare") {
    val instruction = Instruction.decode(Array(OpCode.CP_IMM8.pattern, 0x12.toUByte))
    verifyInstruction[Instruction.CP_IMM8](OpCode.CP_IMM8.pattern, instruction) { cp =>
      assertEquals(cp.imm8, 0x12.toUByte)
    }

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x23.toUByte // A unchanged
        regs.f = 0x40.toUByte // N=1, Z=0, H=0, C=0
      }
    )
  }

  test("CP_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.CP_IMM8.pattern, 0x55.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x55.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x55.toUByte // A unchanged
        regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
      }
    )
  }

  test("CP_IMM8 - borrow/carry") {
    val instruction = Instruction.decode(Array(OpCode.CP_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte // all flags cleared initially
      },
      registerExpect = regs => {
        regs.a = 0x10.toUByte // A unchanged
        regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1
      }
    )
  }

  test("CP_IMM8 - half-borrow only") {
    val instruction = Instruction.decode(Array(OpCode.CP_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      registerSetup = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte
      },
      registerExpect = regs => {
        regs.a = 0x10.toUByte // A unchanged
        regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
      }
    )
  }

}
