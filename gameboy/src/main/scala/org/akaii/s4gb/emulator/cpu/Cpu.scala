package org.akaii.s4gb.emulator.cpu

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.cpu.Cpu.ExecutionMode.Running
import org.akaii.s4gb.emulator.cpu.instructions.Instruction
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

/**
 * Represents the Gameboy CPU
 */
case class Cpu(state: Cpu.State) {
  private var currentInstruction: Option[Instruction] = None

  /**
   * Initializes this CPU to DMG power-up state.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers]]
   */
  def initialize(): Unit = {
    state.registers.initialize()
    state.registers.pc = UShort(0x0100)
    state.registers.sp = UShort(0xFFFE)
  }

  def isStopped: Boolean = state.getExecutionMode == Cpu.ExecutionMode.Stopped

  def tick(): Unit = {
    currentInstruction = currentInstruction.orElse(Some(fetchInstruction()))
    // TODO: Handle interrupts
    state.getExecutionMode match {
      case Cpu.ExecutionMode.Running =>
        state.getIMEFlag.tick()
        currentInstruction.get.execute(state) match {
          case Instruction.ExecutionResult.Completed =>
            currentInstruction = Some(fetchInstruction())
            state.tick(instructionCompleted = true)
          case Instruction.ExecutionResult.Progressing =>
            state.tick(instructionCompleted = false)
        }
      case Cpu.ExecutionMode.Halted =>
        state.getIMEFlag.tick()
        // TODO: HALT BUG
        if(state.isInterruptPending) {
          state.changeExecutionMode(Running)
          // TODO: IF IME=1, handle interrupt
        }
      case Cpu.ExecutionMode.Stopped =>
        () // TODO: Wake up
      case Cpu.ExecutionMode.HardLock =>
        ()
    }
  }

  private def fetchInstruction(): Instruction = {
    val first = state.memory(state.registers.pc)
    val second = state.memory.fetchIfPresent(state.registers.pc + 1.toUShort)
    val third = state.memory.fetchIfPresent(state.registers.pc + 2.toUShort)
    val nextInPC: Array[UByte] = Array(first) ++ second.toArray ++ third.toArray
    Instruction.decode(nextInPC)
  }

}

object Cpu {
  case class State(
    registers: Registers,
    memory: MemoryMap,
    private var imeFlag: IMEFlag = IMEEnabled,
    private var executionMode: ExecutionMode = ExecutionMode.Running,
    private var microStep: Int = 0,
    var cycles: Long = 0L
  ) {
    def setIME(value: Boolean): Unit = (value, imeFlag) match {
      case (true, IMEDisabled) => imeFlag = IMEEnabling
      case (false, _) => imeFlag = IMEDisabled
      case _ => ()
    }

    def imeEnabled: Boolean = imeFlag.enabled

    def getIMEFlag: IMEFlag = imeFlag

    def changeExecutionMode(newMode: ExecutionMode): Unit = {
      executionMode = newMode
    }

    def getMicroStep: Int = microStep

    def getElapsed: Int = microStep + 1

    def getExecutionMode: ExecutionMode = executionMode

    def isInstructionBoundary: Boolean = microStep == 0

    def isInterruptPending: Boolean =
      (memory(Interrupts.Address.INTERRUPT_ENABLE) & memory(Interrupts.Address.INTERRUPT_FLAG)) != UByte(0)

    def tick(instructionCompleted: Boolean): Unit = {
      cycles += 1
      if (instructionCompleted) {
        microStep = 0
      } else {
        microStep += 1
      }
    }
  }

  /**
   * Interrupt Master Enable flag (IME). Used to signal a wait for an interrupt to be handled.
   *
   * @see [[https://gbdev.io/pandocs/Interrupts.html#ime-interrupt-master-enable-flag-write-only]]
   */
  sealed trait IMEFlag {
    def enabled: Boolean

    def tick(): IMEFlag = this
  }

  case object IMEEnabled extends IMEFlag {
    override def enabled: Boolean = true
  }

  case object IMEDisabled extends IMEFlag {
    override def enabled: Boolean = false
  }

  case object IMEEnabling extends IMEFlag {
    override def enabled: Boolean = false

    override def tick(): IMEFlag = IMEEnabled
  }

  sealed trait ExecutionMode

  case object ExecutionMode {
    case object Running extends ExecutionMode

    case object Halted extends ExecutionMode

    case object Stopped extends ExecutionMode

    case object HardLock extends ExecutionMode
  }
}