package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
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
}
