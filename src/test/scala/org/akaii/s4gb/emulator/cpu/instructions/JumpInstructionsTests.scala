package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class JumpInstructionsTests extends InstructionsTest {
  test("JR_IMM8") {
    val basePC: UShort = 0x0100.toUShort
    val nextPC: UShort = basePC + 2.toUShort // PC after fetching the instruction (opcode + offset)

    def jrInstruction(offset: Byte) = Instruction.decode(Array(OpCode.JR_IMM8.pattern, offset.toUByte))

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
}
