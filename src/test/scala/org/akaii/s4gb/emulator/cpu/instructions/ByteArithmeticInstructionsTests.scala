package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.UByte

class ByteArithmeticInstructionsTests extends InstructionsTest {

  test("INC_MEM_HL - normal increment") {
    val opcode: UByte = OpCode.INC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, "INC_MEM_HL(0x34)")
    verifyInstructionOpCode[Instruction.INC_MEM_HL.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, memory) => memory.write(0xC000.toUShort, 0x12.toUByte),
      expectedMemory = memory => memory.write(0xC000.toUShort, 0x13.toUByte),
      expectedRegister = regs => regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=unchanged
    )
  }

  test("INC_MEM_HL - overflow and zero flag set") {
    val opcode: UByte = OpCode.INC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, memory) => memory.write(0xC000.toUShort, 0xFF.toUByte),
      expectedMemory = memory => memory.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0xA0.toUByte // Z=1, N=0, H=1, C=unchanged
    )
  }

  test("INC_MEM_HL - half-carry set") {
    val opcode: UByte = OpCode.INC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, memory) => memory.write(0xC000.toUShort, 0x0F.toUByte),
      expectedMemory = memory => memory.write(0xC000.toUShort, 0x10.toUByte),
      expectedRegister = regs => regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=unchanged
    )
  }

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
        setupRegister = regs => regs(operandParam.toRegister) = 0x12.toUByte,
        expectedRegister = regs => {
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
        setupRegister = regs => regs(operandParam.toRegister) = 0xFF.toUByte,
        expectedRegister = regs => {
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
        setupRegister = regs => regs(operandParam.toRegister) = 0x0F.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x10.toUByte
          regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=unchanged
        }
      )
    }
  }

  test("DEC_MEM_HL - normal decrement") {
    val opcode: UByte = OpCode.DEC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(opcode))

    assertEquals(instruction.toString, "DEC_MEM_HL(0x35)")
    verifyInstructionOpCode[Instruction.DEC_MEM_HL.type](opcode, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, memory) => memory.write(0xC000.toUShort, 0x12.toUByte),
      expectedMemory = memory => memory.write(0xC000.toUShort, 0x11.toUByte),
      expectedRegister = regs => regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=unchanged
    )
  }

  test("DEC_MEM_HL - zero flag set") {
    val opcode: UByte = OpCode.DEC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, memory) => memory.write(0xC000.toUShort, 0x01.toUByte),
      expectedMemory = memory => memory.write(0xC000.toUShort, 0x00.toUByte),
      expectedRegister = regs => regs.f = 0xC0.toUByte // Z=1, N=1, H=1, C=unchanged
    )
  }

  test("DEC_MEM_HL - underflow") {
    val opcode: UByte = OpCode.DEC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, memory) => memory.write(0xC000.toUShort, 0x00.toUByte),
      expectedMemory = memory => memory.write(0xC000.toUShort, 0xFF.toUByte),
      expectedRegister = regs => regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=unchanged
    )
  }

  test("DEC_MEM_HL - half-borrow set") {
    val opcode: UByte = OpCode.DEC_MEM_HL.pattern
    val instruction = Instruction.decode(Array(opcode))

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = 0xC000.toUShort,
      setupMemory = (_, memory) => memory.write(0xC000.toUShort, 0x10.toUByte),
      expectedMemory = memory => memory.write(0xC000.toUShort, 0x0F.toUByte),
      expectedRegister = regs => regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=unchanged
    )
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
        setupRegister = regs => regs(operandParam.toRegister) = 0x12.toUByte,
        expectedRegister = regs => {
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
        setupRegister = regs => regs(operandParam.toRegister) = 0x01.toUByte,
        expectedRegister = regs => {
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
        setupRegister = regs => regs(operandParam.toRegister) = 0x00.toUByte,
        expectedRegister = regs => {
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
        setupRegister = regs => regs(operandParam.toRegister) = 0x10.toUByte,
        expectedRegister = regs => {
          regs(operandParam.toRegister) = 0x0F.toUByte
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=unchanged
        }
      )
    }
  }

  test("ADD_A_R8 - normal addition") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADD_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"ADD_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.ADD_A_R8](opcode, instruction) { add =>
        assertEquals(add.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x12.toUByte
          regs(operandParam.toRegister) = if (operandParam == OpCode.Parameters.R8.A) 0x12.toUByte else 0x23.toUByte
          regs.f = 0xF0.toUByte
        },
        expectedRegister = regs => {
          regs.a = if (operandParam == OpCode.Parameters.R8.A) 0x24.toUByte else 0x35.toUByte
          regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
        }
      )
    }
  }

  test("ADD_A_R8 - zero result (non-zero operands)") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADD_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          if (operandParam == OpCode.Parameters.R8.A) {
            regs.a = 0x80.toUByte
          } else {
            regs.a = 0xFF.toUByte
            regs(operandParam.toRegister) = 0x01.toUByte
          }
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x00.toUByte
          regs.f =
            if (operandParam == OpCode.Parameters.R8.A)
              0x90.toUByte // Z=1, N=0, H=0, C=1
            else
              0xB0.toUByte // Z=1, N=0, H=1, C=1
        }
      )
    }
  }

  test("ADD_A_R8 - overflow/carry") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADD_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          if (operandParam == OpCode.Parameters.R8.A) {
            regs.a = 0x80.toUByte
          } else {
            regs.a = 0xF0.toUByte
            regs(operandParam.toRegister) = 0x20.toUByte
          }
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a =
            if (operandParam == OpCode.Parameters.R8.A) 0x00.toUByte
            else 0x10.toUByte

          regs.f =
            if (operandParam == OpCode.Parameters.R8.A) 0x90.toUByte // Z=1, C=1
            else 0x10.toUByte // Z=0, C=1
        }
      )
    }
  }

  test("ADD_A_R8 - half-carry only") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADD_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          if (operandParam == OpCode.Parameters.R8.A) {
            regs.a = 0x08.toUByte
          } else {
            regs.a = 0x0F.toUByte
            regs(operandParam.toRegister) = 0x01.toUByte
          }
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x10.toUByte
          regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
        }
      )
    }
  }

  test("ADC_A_R8 - normal addition, carry clear") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"ADC_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.ADC_A_R8](opcode, instruction) { adc =>
        assertEquals(adc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x12.toUByte
          regs(operandParam.toRegister) =
            if (operandParam == OpCode.Parameters.R8.A) 0x12.toUByte else 0x23.toUByte
          regs.f = 0x00.toUByte // carry clear
        },
        expectedRegister = regs => {
          regs.a =
            if (operandParam == OpCode.Parameters.R8.A) 0x24.toUByte else 0x35.toUByte
          regs.f = 0x00.toUByte
        }
      )
    }
  }

  test("ADC_A_R8 - addition with initial carry") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x12.toUByte
          regs(operandParam.toRegister) =
            if (operandParam == OpCode.Parameters.R8.A) 0x12.toUByte else 0x23.toUByte
          regs.f = 0x10.toUByte
        },
        expectedRegister = regs => {
          regs.a =
            if (operandParam == OpCode.Parameters.R8.A) 0x25.toUByte else 0x36.toUByte
          regs.f = 0x00.toUByte
        }
      )
    }
  }

  test("ADC_A_R8 - zero result") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          if (operandParam == OpCode.Parameters.R8.A) regs.a = 0x80.toUByte
          else {
            regs.a = 0xFF.toUByte
            regs(operandParam.toRegister) = 0x01.toUByte
          }
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x00.toUByte
          regs.f = // A: Z=1, N=0, H=0, C=1 ; Other: Z=1, N=0, H=1, C=1
            if (operandParam == OpCode.Parameters.R8.A) 0x90.toUByte else 0xB0.toUByte
        }
      )
    }
  }

  test("ADC_A_R8 overflow/carry") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          if (operandParam == OpCode.Parameters.R8.A) regs.a = 0x80.toUByte
          else {
            regs.a = 0xF0.toUByte
            regs(operandParam.toRegister) = 0x20.toUByte
          }
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = if (operandParam == OpCode.Parameters.R8.A) 0x00.toUByte else 0x10.toUByte
          // A: Z=1, N=0, H=0, C=1 ; Other: Z=0, N=0, H=0, C=1
          regs.f = if (operandParam == OpCode.Parameters.R8.A) 0x90.toUByte else 0x10.toUByte
        }
      )
    }
  }

  test("ADC_A_R8 - overflow with initial carry") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          if (operandParam == OpCode.Parameters.R8.A) regs.a = 0x80.toUByte
          else {
            regs.a = 0x7F.toUByte
            regs(operandParam.toRegister) = 0x80.toUByte
          }
          regs.f = 0x10.toUByte
        },
        expectedRegister = regs => {
          regs.a = if (operandParam == OpCode.Parameters.R8.A) 0x01.toUByte else 0x00.toUByte
          // A: Z=0, N=0, H=0, C=1 ; Other: Z=1, N=0, H=1, C=1
          regs.f = if (operandParam == OpCode.Parameters.R8.A) 0x10.toUByte else 0xB0.toUByte
        }
      )
    }
  }

  test("ADC_A_R8 - half-carry only with initial carry") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.ADC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x0F.toUByte
          regs(operandParam.toRegister) =
            if (operandParam == OpCode.Parameters.R8.A) 0x0F.toUByte else 0x01.toUByte
          regs.f = 0x10.toUByte
        },
        expectedRegister = regs => {
          regs.a = if (operandParam == OpCode.Parameters.R8.A) 0x1F.toUByte else 0x11.toUByte
          regs.f = 0x20.toUByte
        }
      )
    }
  }

  test("SUB_A_R8 - normal subtraction") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.SUB_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"SUB_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SUB_A_R8](opcode, instruction) { sbc =>
        assertEquals(sbc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x23.toUByte
          if (operandParam != OpCode.Parameters.R8.A) regs(operandParam.toRegister) = 0x12.toUByte
          regs.f = 0xF0.toUByte
        },
        expectedRegister = regs => {
          regs.a = if (operandParam == OpCode.Parameters.R8.A) 0x00.toUByte else 0x11.toUByte
          // A: Z=1, N=1, H=0, C=0; Other: Z=0, N=1, H=0, C=0
          regs.f = if (operandParam == OpCode.Parameters.R8.A) 0xC0.toUByte else 0x40.toUByte
        }
      )
    }
  }

  test("SUB_A_R8 - zero result") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.SUB_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x23.toUByte
          if (operandParam != OpCode.Parameters.R8.A) regs(operandParam.toRegister) = 0x23.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x00.toUByte
          regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
        }
      )
    }
  }

  test("SUB_A_R8 - borrow/carry") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.SUB_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"SUB_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SUB_A_R8](opcode, instruction) { sub =>
        assertEquals(sub.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x10.toUByte
          regs(operandParam.toRegister) = 0x50.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0xC0.toUByte // 0x10 - 0x50 wraps around
          regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1
        }
      )
    }
  }

  test("SUB_A_R8 - half-borrow only") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.SUB_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"SUB_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SUB_A_R8](opcode, instruction) { sub =>
        assertEquals(sub.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x10.toUByte
          regs(operandParam.toRegister) = 0x01.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x0F.toUByte
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
        }
      )
    }
  }

  test("SBC_A_R8 - normal subtraction, carry clear") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.SBC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"SBC_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SBC_A_R8](opcode, instruction) { sbc =>
        assertEquals(sbc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x23.toUByte
          if (operandParam != OpCode.Parameters.R8.A) regs(operandParam.toRegister) = 0x12.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = if (operandParam == OpCode.Parameters.R8.A) 0x00.toUByte else 0x11.toUByte
          // A: Z=1, N=1, H=0, C=0 ; Other: Z=0, N=1, H=0, C=0
          regs.f = if (operandParam == OpCode.Parameters.R8.A) 0xC0.toUByte else 0x40.toUByte
        }
      )
    }
  }

  test("SBC_A_R8 - subtraction with initial carry") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.SBC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"SBC_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.SBC_A_R8](opcode, instruction) { sbc =>
        assertEquals(sbc.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x23.toUByte
          if (operandParam != OpCode.Parameters.R8.A) regs(operandParam.toRegister) = 0x12.toUByte
          regs.f = 0x10.toUByte // C=1 initially
        },
        expectedRegister = regs => {
          regs.a = if (operandParam == OpCode.Parameters.R8.A) 0xFF.toUByte else 0x10.toUByte
          // A: Z=0, N=1, H=1, C=1 ; Other: Z=0, N=1, H=0, C=0
          regs.f = if (operandParam == OpCode.Parameters.R8.A) 0x70.toUByte else 0x40.toUByte
        }
      )
    }
  }

  test("SBC_A_R8 - zero result") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.SBC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x23.toUByte
          regs(operandParam.toRegister) = 0x22.toUByte
          regs.f = 0x10.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x00.toUByte // 0x23 - 0x22 - 1 = 0
          regs.f = 0xC0.toUByte
        }
      )
    }
  }

  test("SBC_A_R8 - borrow/carry") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.SBC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x10.toUByte
          regs(operandParam.toRegister) = 0x50.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0xC0.toUByte
          regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1
        }
      )
    }
  }

  test("SBC_A_R8 - borrow with initial carry") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.SBC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x10.toUByte
          regs(operandParam.toRegister) = 0x50.toUByte
          regs.f = 0x10.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0xBF.toUByte
          regs.f = 0x70.toUByte // Z=0, N=1, H=1, C=1
        }
      )
    }
  }

  test("SBC_A_R8 - half-borrow with initial carry") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.SBC_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x10.toUByte
          regs(operandParam.toRegister) = 0x01.toUByte
          regs.f = 0x10.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x0E.toUByte
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
        }
      )
    }
  }

  test("CP_A_R8 - normal compare") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CP_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"CP_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.CP_A_R8](opcode, instruction) { cp =>
        assertEquals(cp.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x23.toUByte
          if (operandParam != OpCode.Parameters.R8.A) regs(operandParam.toRegister) = 0x12.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          // A: Z=1, N=1, H=0, C=0 ; Other: Z=0, N=1, H=0, C=0
          regs.f = if (operandParam == OpCode.Parameters.R8.A) 0xC0.toUByte else 0x40.toUByte
        }
      )
    }
  }

  test("CP_A_R8 - zero result") {
    forNonMemHLR8OpCodeParams { operandParam =>
      val opcode: UByte = OpCode.CP_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      verifyInstruction[Instruction.CP_A_R8](opcode, instruction) { cp =>
        assertEquals(cp.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x55.toUByte
          if (operandParam != OpCode.Parameters.R8.A) regs(operandParam.toRegister) = 0x55.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x55.toUByte
          regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
        }
      )
    }
  }

  test("CP_A_R8 - borrow/carry") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.CP_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"CP_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.CP_A_R8](opcode, instruction) { cp =>
        assertEquals(cp.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x10.toUByte
          regs(operandParam.toRegister) = 0x50.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x10.toUByte // A unchanged
          regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1
        }
      )
    }
  }

  test("CP_A_R8 - half-borrow only") {
    forNonMemHLR8OpCodeParamsExcept(OpCode.Parameters.R8.A) { operandParam =>
      val opcode: UByte = OpCode.CP_A_R8.setParam(operandParam -> 0)
      val instruction = Instruction.decode(Array(opcode))

      assertEquals(instruction.toString, f"CP_A_R8(0x${opcode.toInt}%02X)")
      verifyInstruction[Instruction.CP_A_R8](opcode, instruction) { cp =>
        assertEquals(cp.operand, operandParam)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.a = 0x10.toUByte
          regs(operandParam.toRegister) = 0x01.toUByte
          regs.f = 0x00.toUByte
        },
        expectedRegister = regs => {
          regs.a = 0x10.toUByte // A unchanged
          regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
        }
      )
    }
  }

  test("ADD_A_IMM8 - normal addition") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x23.toUByte))
    assertEquals(instruction.toString, "ADD_A_IMM8(0xC623)")
    verifyInstruction[Instruction.ADD_A_IMM8](OpCode.ADD_A_IMM8.pattern, instruction) { add =>
      assertEquals(add.imm8, 0x23.toUByte)
    }

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x12.toUByte
        regs.f = 0xF0.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x35.toUByte // = 0x35
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("ADD_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x00.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x70.toUByte // N=1, H=1, C=1, Z=0;
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte // 0 + 0 = 0
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0
      }
    )
  }

  test("ADD_A_IMM8 - overflow/carry") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x20.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0xF0.toUByte
        regs.f = 0x00.toUByte // clear all flags initially
      },
      expectedRegister = regs => {
        regs.a = 0x10.toUByte // 0xF0 + 0x20 wraps to 0x10
        regs.f = 0x10.toUByte // Z=0, N=0, H=0, C=1
      }
    )
  }

  test("ADD_A_IMM8 - half-carry only") {
    val instruction = Instruction.decode(Array(OpCode.ADD_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x0F.toUByte // 00001111
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      },
      expectedRegister = regs => {
        regs.a = 0x10.toUByte // 0x0F + 0x01 = 0x10
        regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0
      }
    )
  }

  test("ADC_A_IMM8 - normal addition, carry clear") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x23.toUByte))
    assertEquals(instruction.toString, "ADC_A_IMM8(0xCE23)")
    verifyInstruction[Instruction.ADC_A_IMM8](OpCode.ADC_A_IMM8.pattern, instruction) { adc =>
      assertEquals(adc.imm8, 0x23.toUByte)
    }

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x12.toUByte
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      },
      expectedRegister = regs => {
        regs.a = 0x35.toUByte // 0x12 + 0x23 + 0
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("ADC_A_IMM8 - addition with carry-in") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x23.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x12.toUByte
        regs.f = 0x10.toUByte // C=1, other flags cleared
      },
      expectedRegister = regs => {
        regs.a = 0x36.toUByte // 0x12 + 0x23 + 1
        regs.f = 0x00.toUByte // Z=0, N=0, H=0, C=0
      }
    )
  }

  test("ADC_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x00.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x00.toUByte // all flags cleared
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0x80.toUByte // Z=1, N=0, H=0, C=0; lower nibble cleared
      }
    )
  }

  test("ADC_A_IMM8 - overflow/carry") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x90.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x80.toUByte
        regs.f = 0x00.toUByte // all flags cleared
      },
      expectedRegister = regs => {
        regs.a = 0x10.toUByte // low 8 bits of 0x110
        regs.f = 0x10.toUByte // C=1, N=0, H=0, Z=0; lower nibble zero
      }
    )
  }

  test("ADC_A_IMM8 - overflow with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x7F.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x80.toUByte
        regs.f = 0x10.toUByte // C=1 initially, lower nibble cleared
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte // 0x80 + 0x7F + 1 = 0x100 → low 8 bits = 0
        regs.f = 0xB0.toUByte // Z=1, N=0, H=1, C=1; lower nibble zero
      }
    )
  }

  test("ADC_A_IMM8 - half-carry only with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.ADC_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x10.toUByte // C=1 initially, lower nibble zero
      },
      expectedRegister = regs => {
        regs.a = 0x11.toUByte // 0x0F + 0x01 + 1 = 0x11
        regs.f = 0x20.toUByte // Z=0, N=0, H=1, C=0; lower nibble zero
      }
    )
  }

  test("SUB_A_IMM8 - normal subtraction") {
    val instruction = Instruction.decode(Array(OpCode.SUB_A_IMM8.pattern, 0x12.toUByte))
    assertEquals(instruction.toString, "SUB_A_IMM8(0xD612)")
    verifyInstruction[Instruction.SUB_A_IMM8](OpCode.SUB_A_IMM8.pattern, instruction) { sub =>
      assertEquals(sub.imm8, 0x12.toUByte)
    }

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0xF0.toUByte // all flags cleared, lower nibble ignored
      },
      expectedRegister = regs => {
        regs.a = 0x11.toUByte // 0x23 - 0x12 = 0x11
        regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=0
      }
    )
  }

  test("SUB_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.SUB_A_IMM8.pattern, 0x23.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte
        regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
      }
    )
  }

  test("SUB_A_IMM8 - borrow/carry") {
    val instruction = Instruction.decode(Array(OpCode.SUB_A_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0xC0.toUByte // 0x10 - 0x50 wraps to 0xC0
        regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1
      }
    )
  }

  test("SUB_A_IMM8 - half-borrow only") {
    val instruction = Instruction.decode(Array(OpCode.SUB_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x0F.toUByte
        regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
      }
    )
  }

  test("SBC_A_IMM8 - normal subtraction with carry clear") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x12.toUByte))
    assertEquals(instruction.toString, "SBC_A_IMM8(0xDE12)")
    verifyInstruction[Instruction.SBC_A_IMM8](OpCode.SBC_A_IMM8.pattern, instruction) { sbc =>
      assertEquals(sbc.imm8, 0x12.toUByte)
    }

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x00.toUByte // C=0
      },
      expectedRegister = regs => {
        regs.a = 0x11.toUByte // 0x23 - 0x12 - 0 = 0x11
        regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=0
      }
    )
  }

  test("SBC_A_IMM8 - subtraction with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x12.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x10.toUByte // C=1 initially
      },
      expectedRegister = regs => {
        regs.a = 0x10.toUByte // 0x23 - 0x12 - 1 = 0x10
        regs.f = 0x40.toUByte // Z=0, N=1, H=0, C=0
      }
    )
  }

  test("SBC_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x22.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x10.toUByte // C=1
      },
      expectedRegister = regs => {
        regs.a = 0x00.toUByte // 0x23 - 0x22 - 1 = 0
        regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
      }
    )
  }

  test("SBC_A_IMM8 - borrow/carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte // C=0 initially
      },
      expectedRegister = regs => {
        regs.a = 0xC0.toUByte // 0x10 - 0x50 = -0x40 → 0xC0
        regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1; lower nibble zero
      }
    )
  }

  test("SBC_A_IMM8 - borrow with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x10.toUByte // C=1 initially
      },
      expectedRegister = regs => {
        regs.a = 0xBF.toUByte // 0x10 - 0x50 - 1 = -0x41 → 0xBF
        regs.f = 0x70.toUByte // Z=0, N=1, H=0, C=1; lower nibble zero
      }
    )
  }

  test("SBC_A_IMM8 - half-borrow with initial carry") {
    val instruction = Instruction.decode(Array(OpCode.SBC_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x10.toUByte // C=1 initially
      },
      expectedRegister = regs => {
        regs.a = 0x0E.toUByte // 0x10 - 0x01 - 1 = 0x0E
        regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
      }
    )
  }

  test("CP_A_IMM8 - normal compare") {
    val instruction = Instruction.decode(Array(OpCode.CP_A_IMM8.pattern, 0x12.toUByte))
    assertEquals(instruction.toString, "CP_A_IMM8(0xFE12)")
    verifyInstruction[Instruction.CP_A_IMM8](OpCode.CP_A_IMM8.pattern, instruction) { cp =>
      assertEquals(cp.imm8, 0x12.toUByte)
    }

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x23.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x23.toUByte // A unchanged
        regs.f = 0x40.toUByte // N=1, Z=0, H=0, C=0
      }
    )
  }

  test("CP_A_IMM8 - zero result") {
    val instruction = Instruction.decode(Array(OpCode.CP_A_IMM8.pattern, 0x55.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x55.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x55.toUByte // A unchanged
        regs.f = 0xC0.toUByte // Z=1, N=1, H=0, C=0
      }
    )
  }

  test("CP_A_IMM8 - borrow/carry") {
    val instruction = Instruction.decode(Array(OpCode.CP_A_IMM8.pattern, 0x50.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte // all flags cleared initially
      },
      expectedRegister = regs => {
        regs.a = 0x10.toUByte // A unchanged
        regs.f = 0x50.toUByte // Z=0, N=1, H=0, C=1
      }
    )
  }

  test("CP_A_IMM8 - half-borrow only") {
    val instruction = Instruction.decode(Array(OpCode.CP_A_IMM8.pattern, 0x01.toUByte))

    testInstruction(
      instruction,
      setupRegister = regs => {
        regs.a = 0x10.toUByte
        regs.f = 0x00.toUByte
      },
      expectedRegister = regs => {
        regs.a = 0x10.toUByte // A unchanged
        regs.f = 0x60.toUByte // Z=0, N=1, H=1, C=0
      }
    )
  }

}
