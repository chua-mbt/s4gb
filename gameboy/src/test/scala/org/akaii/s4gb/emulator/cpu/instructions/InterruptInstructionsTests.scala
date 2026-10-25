package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.Config
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.cpu.Cpu.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.memorymap.TestMap
import org.akaii.s4gb.extensions.byteops.*
import spire.math.{UByte, UShort}

class InterruptInstructionsTests extends InstructionsTest {
  test("DI") {
    val instruction = Instruction.decode(Array(OpCode.Base.DI.pattern))
    assertEquals(instruction.toString, "DI(0xF3)")
    assertEquals(instruction, Instruction.DI)

    testInstruction(instruction = instruction, setupIME = IMEEnabled, expectedIME = IMEDisabled)
    testInstruction(instruction = instruction, setupIME = IMEEnabling, expectedIME = IMEDisabled)
    testInstruction(instruction = instruction, setupIME = IMEDisabled, expectedIME = IMEDisabled)
  }

  test("EI") {
    val instruction = Instruction.decode(Array(OpCode.Base.EI.pattern))
    assertEquals(instruction.toString, "EI(0xFB)")
    assertEquals(instruction, Instruction.EI)

    testInstruction(instruction = instruction, setupIME = IMEEnabled, expectedIME = IMEEnabled)
    testInstruction(instruction = instruction, setupIME = IMEEnabling, expectedIME = IMEEnabling)
    testInstruction(instruction = instruction, setupIME = IMEDisabled, expectedIME = IMEEnabling)
  }

  test("HALT") {
    val instruction = Instruction.decode(Array(OpCode.Base.HALT.pattern))
    assertEquals(instruction.toString, "HALT(0x76)")
    assertEquals(instruction, Instruction.HALT)

    testInstruction(
      instruction = instruction,
      expectedHaltBugState = HaltBugDormant,
      expectedExecutionMode = ExecutionMode.Halted
    )
  }

  test("HALT - Halt Bug") {
    val instruction = Instruction.decode(Array(OpCode.Base.HALT.pattern))

    testInstruction(
      instruction = instruction,
      setupMemory = (_, mem) => {
        mem.write(Interrupts.Address.INTERRUPT_FLAG, 1.toUByte)
        mem.write(Interrupts.Address.INTERRUPT_ENABLE, 1.toUByte)
      },
      expectedHaltBugState = HaltBugActive,
      expectedExecutionMode = ExecutionMode.Running
    )
  }

  test("HALT - Halt Bug (Disabled)") {
    val instruction = Instruction.decode(Array(OpCode.Base.HALT.pattern))

    testInstruction(
      instruction = instruction,
      config = Config(haltBugEnabled = false),
      setupMemory = (_, mem) => {
        mem.write(Interrupts.Address.INTERRUPT_FLAG, 1.toUByte)
        mem.write(Interrupts.Address.INTERRUPT_ENABLE, 1.toUByte)
      },
      expectedHaltBugState = HaltBugDormant,
      expectedExecutionMode = ExecutionMode.Halted
    )
  }

  test("INTERRUPT_HANDLING services only sources that IE enables") {
    // VBlank and LCDStat are both flagged, but IE allows only LCDStat. Picking from IF alone
    // services VBlank, leaves LCDStat pending, and fires it a second time.
    val state = setupTest(
      setupRegister = registers => registers.sp = UShort(0xFFFE),
      setupMemory = (_, memory) => {
        memory.write(Interrupts.Address.INTERRUPT_FLAG, UByte(0x03))
        memory.write(Interrupts.Address.INTERRUPT_ENABLE, UByte(0x02))
      }
    )

    val finalState = exhaustInstruction(Instruction.INTERRUPT_HANDLING, state)

    assertEquals(finalState.registers.pc, Interrupts.Source.LCDStat.handler)
    // The serviced bit is cleared, the disabled one is left alone.
    assertEquals(finalState.memory(Interrupts.Address.INTERRUPT_FLAG) & UByte(0x03), UByte(0x01))
  }
}
