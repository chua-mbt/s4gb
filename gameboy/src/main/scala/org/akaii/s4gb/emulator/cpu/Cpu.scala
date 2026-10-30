package org.akaii.s4gb.emulator.cpu

import org.akaii.s4gb.emulator.Config
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.cpu.instructions.Instruction
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import org.akaii.s4gb.extensions.byteops.*
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
  def isHardLocked: Boolean = state.getExecutionMode == Cpu.ExecutionMode.HardLock

  def tick(): Unit = {
    state.initializeInstructionOnce()
    state.getExecutionMode.tick(state)
  }
}

object Cpu {
  case class State(
    registers: Registers,
    memory: MemoryMap,
    config: Config,
    private var instruction: Instruction.State = Instruction.State(),
    private var imeFlag: IMEFlag = IMEEnabled,
    private var haltBugState: HaltBugState = HaltBugDormant,
    private var executionMode: ExecutionMode = ExecutionMode.Running,
    private var cycles: Long = 0L
  ) {
    def setIME(value: Boolean): Unit = (value, imeFlag) match {
      case (true, IMEDisabled) => imeFlag = IMEEnabling
      case (false, _) => imeFlag = IMEDisabled
      case _ => ()
    }

    def imeEnabled: Boolean = imeFlag.enabled

    def getIMEFlag: IMEFlag = imeFlag

    def triggerHaltBug(): Unit = haltBugState = HaltBugActive

    def haltBugIsActive: Boolean = haltBugState.active

    def getExecutionMode: ExecutionMode = executionMode

    def changeExecutionMode(newMode: ExecutionMode): Unit =
      executionMode = newMode

    def initializeInstructionOnce(): Unit =
      if(instruction.current == Instruction.INITIAL) setNextInstruction()

    def getInstruction: Instruction = instruction.current

    def setNextInstruction(): Unit = {
      val first = memory(registers.pc)
      val immOffset = if (haltBugIsActive) -1 else 0
      val second = memory.fetchIfPresent(registers.pc + (1 + immOffset).toUShort)
      val third = memory.fetchIfPresent(registers.pc + (2 + immOffset).toUShort)

      // Record imm8/imm16 in instruction state
      second.foreach(instruction.imm8 = _)
      second.zip(third).foreach { case (s, t) =>
        instruction.imm16 = (t.toUShort << 8) | s.toUShort
      }

      instruction.current = Instruction.decode(first, second)
    }

    def getImm8: UByte = instruction.imm8

    def getImm16: UShort = instruction.imm16

    def setScratch8(value: UByte): Unit = instruction.scratchUByte = value
    def getScratch8: UByte = instruction.scratchUByte

    def setScratch16(value: UShort): Unit = instruction.scratchUShort = value
    def getScratch16: UShort = instruction.scratchUShort

    def getMicroStep: Int = instruction.microStep

    def getCycles: Long = cycles

    def getElapsed: Int = instruction.microStep + 1

    def isInstructionBoundary: Boolean = instruction.microStep == 0

    def isInterruptPending: Boolean =
      (memory(Interrupts.Address.INTERRUPT_ENABLE) & memory(Interrupts.Address.INTERRUPT_FLAG)) != UByte.MinValue

    def tick(instructionCompleted: Boolean): Unit = {
      cycles += 1
      if (instructionCompleted) {
        imeFlag = imeFlag.next()
        haltBugState = haltBugState.next()
        instruction.reset()
      } else {
        instruction.microStep += 1
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

  /**
   * Tracking Halt Bug state.
   *
   * @see [[https://gbdev.io/pandocs/halt.html?highlight=halt#halt-bug]]
   */
  sealed trait HaltBugState {
    def active: Boolean

    def next(): HaltBugState = this
  }

  case object HaltBugActive extends HaltBugState {
    override def active: Boolean = true

    override def next(): HaltBugState = HaltBugDormant
  }

  case object HaltBugDormant extends HaltBugState {
    override def active: Boolean = false

    override def next(): HaltBugState = this
  }

  sealed trait ExecutionMode {
    def tick(state: State): Unit = ()
  }

  case object ExecutionMode {
    case object Running extends ExecutionMode {
      override def tick(state: State): Unit = {
        state.getInstruction.execute(state) match {
          case Instruction.ExecutionResult.Completed =>
            state.tick(instructionCompleted = true)
            if (state.isInterruptPending && state.imeEnabled) {
              state.changeExecutionMode(InterruptHandling)
            } else {
              state.setNextInstruction()
            }
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
            state.changeExecutionMode(Running)
          }
        }
      }
    }

    case object Stopped extends ExecutionMode // TODO: Wake up

    case object HardLock extends ExecutionMode

    case object InterruptHandling extends ExecutionMode {
      override def tick(state: State): Unit = {
        Instruction.INTERRUPT_HANDLING.execute(state) match {
          case Instruction.ExecutionResult.Completed =>
            state.tick(instructionCompleted = true)
            state.setNextInstruction()
            state.changeExecutionMode(Running)
          case Instruction.ExecutionResult.Progressing =>
            state.tick(instructionCompleted = false)
        }
      }
    }
  }
}