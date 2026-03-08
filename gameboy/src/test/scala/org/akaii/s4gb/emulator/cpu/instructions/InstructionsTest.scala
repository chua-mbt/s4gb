package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Cpu.{IMEEnabled, IMEFlag}
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.Instruction.MCycle
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.{MemoryMap, TestMap, copyTo}
import spire.math.{UByte, UShort, e}

import scala.reflect.ClassTag

abstract class InstructionsTest extends FunSuite {

  def setupTest(
    setupRegister: Registers => Unit = _ => (),
    setupMemory: (Registers, TestMap) => Unit = (_, _) => (),
    initialIME: IMEFlag = IMEEnabled
  ): Cpu.State = {
    val registers = Registers()
    val memory = TestMap()
    setupRegister(registers)
    setupMemory(registers, memory)
    Cpu.State(registers, memory, initialIME)
  }

  def setupExpected(
    initialState: Cpu.State,
    instruction: Instruction,
    expectedRegisters: Registers => Unit = _ => (),
    expectedMemory: TestMap => Unit = _ => (),
    expectedIME: IMEFlag = IMEEnabled
  ): Cpu.State = {
    val registers = Registers()
    val memory = TestMap()

    initialState.registers.copyTo(registers)
    initialState.memory.asInstanceOf[TestMap].copyTo(memory)

    registers.pc = instruction.bytes.toUShort

    expectedRegisters(registers)
    expectedMemory(memory)

    Cpu.State(registers, memory, expectedIME)
  }

  @annotation.tailrec
  final def exhaustInstruction(instruction: Instruction, state: Cpu.State): Cpu.State =
    instruction.execute(state) match {
      case Instruction.ExecutionResult.Completed =>
        state
      case Instruction.ExecutionResult.Progressing =>
        state.setMicroStep(state.getMicroStep + 1)
        exhaustInstruction(instruction, state)
    }

  protected def verifyInstruction[T <: Instruction : ClassTag](
    opCode: UByte,
    instruction: Instruction,
    clue: String = ""
  )(verifications: T => Unit = (_: T) => ())(implicit loc: Location): Unit = {
    assert(summon[ClassTag[T]].runtimeClass.isInstance(instruction), clue)
    assertEquals(instruction.opCode, opCode, clue)
    if (instruction.cycles != MCycle.Undefined) assertEquals(instruction.micro.length, instruction.cycles.maxCost)
    verifications(instruction.asInstanceOf[T])
  }

  protected def verifyFinalState(
    finalState: Cpu.State,
    instruction: Instruction,
    expectedPC: UShort,
    expectedExecutionMode: Cpu.ExecutionMode,
    expectedElapsed: Option[Int],
    expectedState: Cpu.State,
    clue: String
  )(implicit loc: Location): Unit = {
    assert(instruction.cycles.withinCost(finalState.getElapsed), clue)
    expectedElapsed.foreach(expected => assertEquals(finalState.getElapsed, expected, clue))
    assertEquals(finalState.getExecutionMode, expectedExecutionMode, clue)
    assertEquals(finalState.registers.pc, expectedPC, clue)
    assertEquals(finalState.registers, expectedState.registers, clue)
    assertEquals(finalState.memory, expectedState.memory, clue)
    assertEquals(finalState.getIMEFlag, expectedState.getIMEFlag, clue)
  }

  protected def testInstruction(
    instruction: Instruction,
    setupRegister: Registers => Unit = _ => (),
    setupMemory: (Registers, TestMap) => Unit = (_, _) => (),
    setupIME: IMEFlag = IMEEnabled,
    expectedRegister: Registers => Unit = _ => (),
    expectedMemory: TestMap => Unit = _ => (),
    expectedIME: IMEFlag = IMEEnabled,
    expectedExecutionMode: Cpu.ExecutionMode = Cpu.ExecutionMode.Running,
    expectedPC: Option[UShort] = None,
    expectedElapsed: Option[Int] = None,
    clue: String = ""
  )(implicit loc: Location): Unit = {
    val initialState = setupTest(setupRegister, setupMemory, setupIME)
    val expectedState = setupExpected(initialState, instruction, expectedRegister, expectedMemory, expectedIME)
    val finalState = exhaustInstruction(instruction, initialState)
    verifyFinalState(finalState, instruction, expectedPC.getOrElse(instruction.bytes.toUShort), expectedExecutionMode,
      expectedElapsed, expectedState, clue)
  }

  protected def forNonSPR16OpCodeParams(test: OpCode.Parameters.R16 => Unit): Unit =
    OpCode.Parameters.R16.nonSPValues.foreach(test)

  protected def forR16MemOpCodeParams(test: OpCode.Parameters.R16Mem => Unit): Unit =
    OpCode.Parameters.R16Mem.values.foreach(test)

  protected def forR16StackOpCodeParams(test: OpCode.Parameters.R16Stack => Unit): Unit =
    OpCode.Parameters.R16Stack.values.foreach(test)

  protected def forNonMemHLR8OpCodeParams(test: OpCode.Parameters.R8 => Unit): Unit =
    OpCode.Parameters.R8.nonMemHLValues.foreach(test)

  protected def forNonMemHLR8OpCodeParamsExcept(r8: OpCode.Parameters.R8*)(test: OpCode.Parameters.R8 => Unit): Unit =
    OpCode.Parameters.R8.nonMemHLValues.filterNot(r8.contains).foreach(test)

  protected def forNonMemHLR8OpCodeParamPairs(test: (OpCode.Parameters.R8, OpCode.Parameters.R8) => Unit): Unit =
    for {
      source <- OpCode.Parameters.R8.nonMemHLValues
      dest <- OpCode.Parameters.R8.nonMemHLValues
    } test(source, dest)

  protected def forCondOpCodeParams(test: OpCode.Parameters.Condition => Unit): Unit =
    OpCode.Parameters.Condition.values.foreach(test)
}