package org.akaii.s4gb.emulator.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{MemoryMap, TestMap, copyTo}
import spire.math.{UByte, UShort}

import scala.reflect.ClassTag

abstract class InstructionsTest extends FunSuite {
  def setupTest(
    registerSetup: Registers => Unit = _ => (),
    memorySetup: (Registers, TestMap) => Unit = (_, _) => ()
  ): Instruction.State = {
    val registers = Registers()
    val memory = TestMap()
    registerSetup(registers)
    memorySetup(registers, memory)
    Instruction.State(registers, memory)
  }

  def setupExpected(
    initialState: Instruction.State,
    instruction: Instruction,
    registerExpect: Registers => Unit = _ => {},
    memoryExpect: TestMap => Unit = _ => {}
  ): Instruction.State = {
    val expectedRegisters = Registers()
    val expectedMemory = TestMap()

    initialState.registers.copyTo(expectedRegisters)
    initialState.memory.asInstanceOf[TestMap].copyTo(expectedMemory)

    expectedRegisters.pc = instruction.bytes.toUShort
    expectedRegisters.sp = instruction.cycles.toUShort

    registerExpect(expectedRegisters)
    memoryExpect(expectedMemory)

    Instruction.State(expectedRegisters, expectedMemory)
  }

  @annotation.tailrec
  final def exhaustInstruction(instruction: Instruction, state: Instruction.State): Instruction.State = {
    if (instruction.execute(state))
      state
    else
      exhaustInstruction(instruction, state.copy(elapsed = state.elapsed + 1))
  }

  protected def verifyInstruction[T <: Instruction : ClassTag](
    opCode: UByte,
    instruction: Instruction
  )(verifications: T => Unit = (_: T) => ()): Unit = {
    assert(summon[ClassTag[T]].runtimeClass.isInstance(instruction))
    assertEquals(instruction.opCode, opCode)
    assertEquals(instruction.micro.map(_.cycles).sum, instruction.cycles)
    verifications(instruction.asInstanceOf[T])
  }

  protected def verifyFinalState(
    finalState: Instruction.State,
    instruction: Instruction,
    expectedState: Instruction.State
  ): Unit = {
    assertEquals(finalState.elapsed, instruction.cycles)
    assertEquals(finalState.elapsed, expectedState.registers.sp.toInt)
    assertEquals(finalState.registers.sp, instruction.cycles.toUShort)
    assertEquals(finalState.registers.pc, instruction.bytes.toUShort)
    assertEquals(finalState.registers, expectedState.registers)
    assertEquals(finalState.memory.asInstanceOf[TestMap], expectedState.memory.asInstanceOf[TestMap])
  }

  protected def testInstruction(
    instruction: Instruction,
    registerSetup: Registers => Unit = _ => (),
    memorySetup: (Registers, TestMap) => Unit = (_, _) => (),
    registerExpect: Registers => Unit = _ => (),
    memoryExpect: TestMap => Unit = _ => ()
  ): Unit = {
    val initialState = setupTest(registerSetup, memorySetup)
    val expectedState = setupExpected(initialState, instruction, registerExpect, memoryExpect)
    val finalState = exhaustInstruction(instruction, initialState)
    verifyFinalState(finalState, instruction, expectedState)
  }

  protected def forAllR16(test: Registers.R16 => Unit): Unit =
    Registers.R16.values.filterNot(_ == Registers.R16.AF).foreach(test)

  protected def forAllR8(test: Registers.R8 => Unit): Unit =
    Registers.R8.values.filterNot(_ == Registers.R8.F).foreach(test)

  protected def forAllR8Pairs(test: (Registers.R8, Registers.R8) => Unit): Unit =
    for {
      source <- Registers.R8.values.filterNot(_ == Registers.R8.F)
      dest <- Registers.R8.values.filterNot(_ == Registers.R8.F)
    } test(source, dest)
}