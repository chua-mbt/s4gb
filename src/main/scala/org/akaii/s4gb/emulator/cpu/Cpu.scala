package org.akaii.s4gb.emulator.cpu

import org.akaii.s4gb.emulator.MemoryMap
import org.akaii.s4gb.emulator.cpu.instructions.Instruction

/**
 * Represents the Gameboy CPU
 */
case class Cpu(state: Cpu.State, initialInstruction: Instruction) {
  private var currentInstruction: Instruction = initialInstruction

  def isStopped: Boolean = state.getExecutionMode == Cpu.ExecutionMode.Stopped

  def tick(): Unit = {
    // TODO: Handle interrupts
    state.getExecutionMode match {
      case Cpu.ExecutionMode.Running =>
        state.getIMEFlag.tick()
        currentInstruction.execute(state) match {
          case Instruction.ExecutionResult.Completed =>
            val nextInPC = state.memory.read(state.registers.pc, 3)
            currentInstruction = Instruction.decode(nextInPC)
            state.setMicroStep(0)
          case Instruction.ExecutionResult.Progressing =>
            state.setMicroStep(state.getMicroStep + 1)
        }
      case Cpu.ExecutionMode.Halted =>
        state.getIMEFlag.tick()
        () // TODO: Depends on IME
      case Cpu.ExecutionMode.Stopped =>
        () // TODO: Wake up
    }
  }
}

object Cpu {
  case class State(
    registers: Registers,
    memory: MemoryMap,
    private var imeFlag: IMEFlag = IMEEnabled,
    private var executionMode: ExecutionMode = ExecutionMode.Running,
    private var microStep: Int = 0
  ) {
    def setIME(value: Boolean): Unit = (value, imeFlag) match {
      case (true, IMEDisabled) => imeFlag = IMEEnabling
      case (false, _) => imeFlag = IMEDisabled
      case _ => ()
    }

    def imeEnabled: Boolean = imeFlag.enabled
    def getIMEFlag: IMEFlag = imeFlag
    def changeExecutionMode(newMode: ExecutionMode): Unit = { executionMode = newMode }
    def setMicroStep(cycles: Int): Unit = { microStep = cycles }
    def getMicroStep: Int = microStep
    def getElapsed: Int = microStep + 1
    def getExecutionMode: ExecutionMode = executionMode
  }

  sealed trait IMEFlag {
    def enabled: Boolean
    def tick(): IMEFlag = this
  }
  case object IMEEnabled extends IMEFlag { override def enabled: Boolean = true }
  case object IMEDisabled extends IMEFlag { override def enabled: Boolean = false }
  case object IMEEnabling extends IMEFlag {
    override def enabled: Boolean = false
    override def tick(): IMEFlag = IMEEnabled
  }

  sealed trait ExecutionMode
  case object ExecutionMode {
    case object Running extends ExecutionMode
    case object Halted extends ExecutionMode
    case object Stopped extends ExecutionMode
  }
}