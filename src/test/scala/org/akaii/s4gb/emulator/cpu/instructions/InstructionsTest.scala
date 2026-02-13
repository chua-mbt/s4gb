package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Cpu.{IMEEnabled, IMEFlag}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import org.akaii.s4gb.emulator.{MemoryMap, TestMap, copyTo}
import spire.math.{UByte, UShort}

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
    if (instruction.execute(state)) {
      state
    } else {
      state.setMicroStep(state.getMicroStep + 1)
      exhaustInstruction(instruction, state)
    }

  protected def verifyInstruction[T <: Instruction : ClassTag](
    opCode: UByte,
    instruction: Instruction
  )(verifications: T => Unit = (_: T) => ())(implicit loc: Location): Unit = {
    assert(summon[ClassTag[T]].runtimeClass.isInstance(instruction))
    assertEquals(instruction.opCode, opCode)
    assertEquals(instruction.micro.length, instruction.cycles.maxCost)
    verifications(instruction.asInstanceOf[T])
  }

  protected def verifyInstructionOpCode[T <: Instruction : ClassTag](
    opCode: UByte,
    instruction: Instruction
  )(implicit loc: Location): Unit = verifyInstruction[T](opCode, instruction)()

  protected def verifyFinalState(
    finalState: Cpu.State,
    instruction: Instruction,
    expectedPC: UShort,
    expectedElapsed: Option[Int],
    expectedState: Cpu.State
  )(implicit loc: Location): Unit = {
    assert(instruction.cycles.withinCost(finalState.getElapsed))
    expectedElapsed.foreach(expected => assertEquals(finalState.getElapsed, expected))
    assertEquals(finalState.registers.pc, expectedPC)
    assertEquals(finalState.registers, expectedState.registers)
    assertEquals(finalState.memory, expectedState.memory)
  }

  protected def testInstruction(
    instruction: Instruction,
    setupRegister: Registers => Unit = _ => (),
    setupMemory: (Registers, TestMap) => Unit = (_, _) => (),
    setupIME: IMEFlag = IMEEnabled,
    expectedRegister: Registers => Unit = _ => (),
    expectedMemory: TestMap => Unit = _ => (),
    expectedIME: IMEFlag = IMEEnabled,
    expectedPC: Option[UShort] = None,
    expectedElapsed: Option[Int] = None
  )(implicit loc: Location): Unit = {
    val initialState = setupTest(setupRegister, setupMemory, setupIME)
    val expectedState = setupExpected(initialState, instruction, expectedRegister, expectedMemory, expectedIME)
    val finalState = exhaustInstruction(instruction, initialState)
    verifyFinalState(finalState, instruction, expectedPC.getOrElse(instruction.bytes.toUShort),
      expectedElapsed, expectedState)
  }

  protected def forNonSPR16OpCodeParams(test: OpCode.Parameters.R16 => Unit): Unit =
    OpCode.Parameters.R16.nonSPValues.foreach(test)

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