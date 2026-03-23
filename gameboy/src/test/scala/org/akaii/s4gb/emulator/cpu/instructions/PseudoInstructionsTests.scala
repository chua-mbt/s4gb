package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.cpu.Cpu
import org.akaii.s4gb.emulator.cpu.Cpu.ExecutionMode
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}

class PseudoInstructionsTests extends InstructionsTest {
  test("HOLE") {
    OpCode.HOLES.foreach { hole =>
      val instruction = Instruction.decode(Array(hole))
      assertEquals(instruction.toString, f"HOLE(0x${hole.toInt}%02X)")
      verifyInstruction[Instruction.HOLE](hole, instruction)
      testInstruction(instruction = instruction, expectedExecutionMode = ExecutionMode.HardLock)
    }
  }

  test("INTERRUPT_HANDLING") {
    Interrupts.Source.values.foreach { source =>
      val state = setupTest(
        setupRegister = regs => regs.sp = 0xFFFE.toUShort,
        setupMemory = (_, memory) => {
          val mask = (1 << source.bit).toUByte
          memory.write(Interrupts.Address.INTERRUPT_FLAG, mask)
        },
        initialIME = Cpu.IMEEnabled
      )

      val finalState = exhaustInstruction(Instruction.INTERRUPT_HANDLING, state)

      val expectedSP = (0xFFFE - 2).toUShort
      val expectedPC = source.handler
      val expectedIF = 0.toUByte

      assertEquals(finalState.registers.sp, expectedSP, s"SP after interrupt for $source")
      assertEquals(finalState.registers.pc, expectedPC, s"PC after interrupt for $source")
      assertEquals(finalState.getIMEFlag.enabled, false, s"IME cleared for $source")
      assertEquals(finalState.memory(Interrupts.Address.INTERRUPT_FLAG), expectedIF, s"IF cleared for $source")
      assertEquals(finalState.getElapsed, 5, s"Elapsed cycles for interrupt $source")
    }
  }
}
