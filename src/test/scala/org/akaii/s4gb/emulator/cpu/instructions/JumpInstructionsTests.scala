package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Cpu.IMEEnabled
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{TestMap, setParam}
import spire.math.{UByte, UShort}

class JumpInstructionsTests extends InstructionsTest {
  import JumpInstructionsTests.*

  test("JR_IMM8") {
    val basePC: UShort = 0x0100.toUShort
    val nextPC: UShort = basePC + 2.toUShort // PC after fetching the instruction (opcode + offset)

    def jrInstruction(offset: Byte) = {
      val unsigned = offset.toUByte
      val instruction = Instruction.decode(Array(OpCode.JR_IMM8.pattern, unsigned))
      verifyInstruction[Instruction.JR_IMM8](OpCode.JR_IMM8.pattern, instruction) { jr => assertEquals(jr.imm8, unsigned) }
      instruction
    }

    // No-op jump (offset = 0)
    testInstruction(
      jrInstruction(0),
      setupRegister = regs => regs.pc = basePC,
      expectedRegister = regs => regs.pc = nextPC,
      expectedPC = Some(nextPC)
    )

    // Forward small jump (offset = 5)
    testInstruction(
      jrInstruction(5),
      setupRegister = regs => regs.pc = basePC,
      expectedRegister = regs => regs.pc = nextPC + 5.toUShort,
      expectedPC = Some(nextPC + 5.toUShort)
    )

    // Forward max jump (offset = 127)
    testInstruction(
      jrInstruction(127),
      setupRegister = regs => regs.pc = basePC,
      expectedRegister = regs => regs.pc = nextPC + 127.toUShort,
      expectedPC = Some(nextPC + 127.toUShort)
    )

    // Backward small jump (offset = -1)
    testInstruction(
      jrInstruction(-1),
      setupRegister = regs => regs.pc = basePC,
      expectedRegister = regs => regs.pc = nextPC - 1.toUShort,
      expectedPC = Some(nextPC - 1.toUShort)
    )

    // Backward max jump (offset = -128)
    testInstruction(
      jrInstruction(-128),
      setupRegister = regs => regs.pc = basePC,
      expectedRegister = regs => regs.pc = nextPC - 128.toUShort,
      expectedPC = Some(nextPC - 128.toUShort)
    )

    // Infinite loop (offset = -2, points back to itself)
    testInstruction(
      jrInstruction(-2),
      setupRegister = regs => regs.pc = basePC,
      expectedRegister = regs => regs.pc = basePC,
      expectedPC = Some(basePC)
    )
  }

  test("JR_COND_IMM8 - condition met") {
    val basePC: UShort = 0x0100.toUShort
    val nextPC: UShort = basePC + 2.toUShort

    forCondOpCodeParams { cond =>
      def jrInstruction(offset: Byte) = {
        val unsigned = offset.toUByte
        val opcode: UByte = OpCode.JR_COND_IMM8.setParam(cond -> 3)
        val instruction = Instruction.decode(Array(opcode, unsigned))
        verifyInstruction[Instruction.JR_COND_IMM8](opcode, instruction) { jr =>
          assertEquals(jr.condition, cond)
          assertEquals(jr.imm8, unsigned)
        }
        instruction
      }

      // No-op jump (offset = 0)
      testInstruction(
        jrInstruction(0),
        setupRegister = regs => {
          regs.pc = basePC
          setupFlagForCondition(regs, cond)
        },
        expectedRegister = regs => regs.pc = nextPC,
        expectedPC = Some(nextPC),
        expectedElapsed = Some(3)
      )

      // Forward small jump (offset = 5)
      testInstruction(
        jrInstruction(5),
        setupRegister = regs => {
          regs.pc = basePC
          setupFlagForCondition(regs, cond)
        },
        expectedRegister = regs => regs.pc = nextPC + 5.toUShort,
        expectedPC = Some(nextPC + 5.toUShort),
        expectedElapsed = Some(3)
      )

      // Forward max jump (offset = 127)
      testInstruction(
        jrInstruction(127),
        setupRegister = regs => {
          regs.pc = basePC
          setupFlagForCondition(regs, cond)
        },
        expectedRegister = regs => regs.pc = nextPC + 127.toUShort,
        expectedPC = Some(nextPC + 127.toUShort),
        expectedElapsed = Some(3)
      )

      // Backward small jump (offset = -1)
      testInstruction(
        jrInstruction(-1),
        setupRegister = regs => {
          regs.pc = basePC
          setupFlagForCondition(regs, cond)
        },
        expectedRegister = regs => regs.pc = nextPC - 1.toUShort,
        expectedPC = Some(nextPC - 1.toUShort),
        expectedElapsed = Some(3)
      )

      // Backward max jump (offset = -128)
      testInstruction(
        jrInstruction(-128),
        setupRegister = regs => {
          regs.pc = basePC
          setupFlagForCondition(regs, cond)
        },
        expectedRegister = regs => regs.pc = nextPC - 128.toUShort,
        expectedPC = Some(nextPC - 128.toUShort),
        expectedElapsed = Some(3)
      )

      // Infinite loop (offset = -2)
      testInstruction(
        jrInstruction(-2),
        setupRegister = regs => {
          regs.pc = basePC
          setupFlagForCondition(regs, cond)
        },
        expectedRegister = regs => regs.pc = basePC,
        expectedPC = Some(basePC),
        expectedElapsed = Some(3)
      )
    }
  }

  test("JR_COND_IMM8 - condition not met") {
    val basePC: UShort = 0x0100.toUShort
    val offset: Byte = 5

    forCondOpCodeParams { cond =>
      val opcode: UByte = OpCode.JR_COND_IMM8.setParam(cond -> 3)
      val instruction = Instruction.decode(Array(opcode, offset.toUByte))

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.pc = basePC
          setupFlagToFailCondition(regs, cond)
        },
        expectedRegister = regs => regs.pc = basePC + 2.toUShort, // PC should only advance past opcode+offset
        expectedPC = Some(basePC + 2.toUShort)
      )
    }
  }

  test("RET_COND - condition met") {
    val returnAddress: UShort = 0x1234.toUShort
    val initialSP: UShort = 0xFFFE.toUShort

    forCondOpCodeParams { cond =>
      val opcode: UByte = OpCode.RET_COND.setParam(cond -> 3)
      val instruction = Instruction.decode(Array(opcode))

      verifyInstruction[Instruction.RET_COND](opcode, instruction) { ret =>
        assertEquals(ret.condition, cond)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.sp = initialSP
          setupFlagForCondition(regs, cond)
        },
        setupMemory = (regs, memory) => {
          memory.write(initialSP, returnAddress.loByte)
          memory.write(initialSP + 1.toUShort, returnAddress.hiByte)
        },
        expectedRegister = regs => {
          regs.pc = returnAddress
          regs.sp = 0x0000.toUShort
        },
        expectedPC = Some(returnAddress),
        expectedElapsed = Some(5)
      )
    }
  }

  test("RET_COND - condition not met") {
    val initialSP: UShort = 0xFFFE.toUShort

    forCondOpCodeParams { cond =>
      val opcode: UByte = OpCode.RET_COND.setParam(cond -> 3)
      val instruction = Instruction.decode(Array(opcode))

      verifyInstruction[Instruction.RET_COND](opcode, instruction) { ret =>
        assertEquals(ret.condition, cond)
      }

      testInstruction(
        instruction,
        setupRegister = regs => {
          regs.sp = initialSP
          setupFlagToFailCondition(regs, cond)
        },
        expectedRegister = regs => regs.sp = initialSP, // SP unchanged
        expectedPC = Some(instruction.bytes.toUShort),  // only advance past opcode
        expectedElapsed = Some(2)
      )
    }
  }

  test("RET") {
    val returnAddress: UShort = 0x1234.toUShort
    val initialSP: UShort = 0xFFFE.toUShort

    val instruction = Instruction.decode(Array(OpCode.RET.pattern))
    verifyInstructionOpCode[Instruction.RET.type](OpCode.RET.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.sp = initialSP,
      setupMemory = (regs, memory) => {
        memory.write(initialSP, returnAddress.loByte)
        memory.write(initialSP + 1.toUShort, returnAddress.hiByte)
      },
      expectedRegister = regs => {
        regs.pc = returnAddress
        regs.sp = 0x0000.toUShort // incremented twice
      },
      expectedPC = Some(returnAddress)
    )
  }

  test("RETI") {
    val returnAddress: UShort = 0x1234.toUShort
    val initialSP: UShort = 0xFFFE.toUShort

    val instruction = Instruction.decode(Array(OpCode.RET.pattern))
    verifyInstructionOpCode[Instruction.RET.type](OpCode.RET.pattern, instruction)

    testInstruction(
      instruction,
      setupRegister = regs => regs.sp = initialSP,
      setupMemory = (regs, memory) => {
        memory.write(initialSP, returnAddress.loByte)
        memory.write(initialSP + 1.toUShort, returnAddress.hiByte)
      },
      expectedRegister = regs => {
        regs.pc = returnAddress
        regs.sp = 0x0000.toUShort // incremented twice
      },
      expectedPC = Some(returnAddress),
      expectedIME = IMEEnabled
    )
  }

  test("JP_HL") {
    val instruction = Instruction.decode(Array(OpCode.JP_HL.pattern))
    verifyInstructionOpCode[Instruction.JP_HL.type](OpCode.JP_HL.pattern, instruction)

    val targetAddress = 0x1234.toUShort

    testInstruction(
      instruction,
      setupRegister = regs => regs.hl = targetAddress,
      expectedRegister = regs => regs.pc = targetAddress,
      expectedPC = Some(targetAddress),
    )
  }

}

object JumpInstructionsTests {
  def setupFlagForCondition(regs: Registers, cond: OpCode.Parameters.Condition): Unit = cond match {
    case OpCode.Parameters.Condition.NZ => regs.flags.z = false
    case OpCode.Parameters.Condition.Z  => regs.flags.z = true
    case OpCode.Parameters.Condition.NC => regs.flags.c = false
    case OpCode.Parameters.Condition.C  => regs.flags.c = true
  }

  def setupFlagToFailCondition(regs: Registers, cond: OpCode.Parameters.Condition): Unit = cond match {
    case OpCode.Parameters.Condition.NZ => regs.flags.z = true
    case OpCode.Parameters.Condition.Z  => regs.flags.z = false
    case OpCode.Parameters.Condition.NC => regs.flags.c = true
    case OpCode.Parameters.Condition.C  => regs.flags.c = false
  }
}
