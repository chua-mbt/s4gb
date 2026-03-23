package org.akaii.s4gb.emulator.cpu

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.cpu.instructions.Instruction
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

/**
 * Represents the Gameboy CPU
 */
case class Cpu(state: Cpu.State) {
  import Cpu.*

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
    state.initializeInstructionOnce()
    state.getExecutionMode.tick(state)
  }
}

object Cpu {
  case class State(
    registers: Registers,
    memory: MemoryMap,
    private var imeFlag: IMEFlag = IMEEnabled,
    private var executionMode: ExecutionMode = ExecutionMode.Running,
    private var microStep: Int = 0,
    private var cycles: Long = 0L,
    private var currentInstruction: Option[Instruction] = None
  ) {
    def setIME(value: Boolean): Unit = (value, imeFlag) match {
      case (true, IMEDisabled) => imeFlag = IMEEnabling
      case (false, _) => imeFlag = IMEDisabled
      case _ => ()
    }

    def imeEnabled: Boolean = imeFlag.enabled

    def getIMEFlag: IMEFlag = imeFlag

    def getExecutionMode: ExecutionMode = executionMode

    def changeExecutionMode(newMode: ExecutionMode): Unit =
      executionMode = newMode

    def initializeInstructionOnce(): Unit =
      currentInstruction = currentInstruction.orElse(Some(fetchInstruction()))

    def getInstruction: Instruction = currentInstruction.get

    def setNextInstruction(): Unit =
      currentInstruction = Some(fetchInstruction())

    def getMicroStep: Int = microStep

    def getElapsed: Int = microStep + 1

    def isInstructionBoundary: Boolean = microStep == 0

    def isInterruptPending: Boolean =
      (memory(Interrupts.Address.INTERRUPT_ENABLE) & memory(Interrupts.Address.INTERRUPT_FLAG)) != UByte(0)

    def tick(instructionCompleted: Boolean): Unit = {
      cycles += 1
      imeFlag = imeFlag.next()
      if (instructionCompleted) {
        microStep = 0
      } else {
        microStep += 1
      }
    }

    private def fetchInstruction(): Instruction = {
      val first = memory(registers.pc)
      val second = memory.fetchIfPresent(registers.pc + 1.toUShort)
      val third = memory.fetchIfPresent(registers.pc + 2.toUShort)
      val nextInPC: Array[UByte] = Array(first) ++ second.toArray ++ third.toArray
      Instruction.decode(nextInPC)
    }
  }

  /**
   * Interrupt Master Enable flag (IME). Used to signal a wait for an interrupt to be handled.
   *
   * @see [[https://gbdev.io/pandocs/Interrupts.html#ime-interrupt-master-enable-flag-write-only]]
   */
  sealed trait IMEFlag {
    def enabled: Boolean

    def next(): IMEFlag = this
  }

  case object IMEEnabled extends IMEFlag {
    override def enabled: Boolean = true
  }

  case object IMEDisabled extends IMEFlag {
    override def enabled: Boolean = false
  }

  case object IMEEnabling extends IMEFlag {
    override def enabled: Boolean = false

    override def next(): IMEFlag = IMEEnabled
  }

  sealed trait ExecutionMode { def tick(state: State): Unit = () }

  case object ExecutionMode {
    case object Running extends ExecutionMode {
      override def tick(state: State): Unit = {
        state.getInstruction.execute(state) match {
          case Instruction.ExecutionResult.Completed =>
            if (state.isInterruptPending && state.imeEnabled) {
              state.changeExecutionMode(InterruptHandling)
            } else {
              state.setNextInstruction()
            }
            state.tick(instructionCompleted = true)
          case Instruction.ExecutionResult.Progressing =>
            state.tick(instructionCompleted = false)
        }
      }
    }

    case object Halted extends ExecutionMode {
      override def tick(state: State): Unit = {
        val noInstruction = true
        state.tick(noInstruction)
        if (state.isInterruptPending) {
          if (state.imeEnabled) {
            state.changeExecutionMode(InterruptHandling)
          } else {
            // TODO: HALT BUG
            state.changeExecutionMode(Running)
          }
        }
      }
    }

    case object Stopped extends ExecutionMode // TODO: Wake up

    case object HardLock extends ExecutionMode // TODO: Is this done?

    case object InterruptHandling extends ExecutionMode {
      override def tick(state: State): Unit = {
        Instruction.INTERRUPT_HANDLING.execute(state) match {
          case Instruction.ExecutionResult.Completed =>
            state.setNextInstruction()
            state.changeExecutionMode(Running)
            state.tick(instructionCompleted = true)
          case Instruction.ExecutionResult.Progressing =>
            state.tick(instructionCompleted = false)
        }
      }
    }
  }
}