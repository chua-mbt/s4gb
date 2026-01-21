package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}
import utest.*

object InterruptInstructionsTests extends TestSuite {

  val tests: Tests = Tests {
    test("HALT") {
      val instruction = Instruction.decode(Array(OpCode.HALT.pattern))
      assert(instruction == Instruction.HALT)
      assert(instruction.toString == "HALT(0x76)")

      // TODO : instruction.execute.apply.tupled(state)
    }
  }
}
