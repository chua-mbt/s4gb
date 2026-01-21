package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.TestMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}
import utest.*

object MiscInstructionsTests extends TestSuite {

  val tests: Tests = Tests {
    test("NOP") {
      val instruction = Instruction.decode(Array(OpCode.NOP.pattern))
      assert(instruction == Instruction.NOP)
      assert(instruction.toString == "NOP(0x00)")

      val state = (Registers(), TestMap())
      instruction.execute.apply.tupled(state)

      val expectedRegisters = Registers()
      expectedRegisters.pc = 1.toUShort
      expectedRegisters.sp = 1.toUShort

      assert(state == (expectedRegisters, TestMap())) // NOP should not change state besides PC and SP
    }
  }
}
