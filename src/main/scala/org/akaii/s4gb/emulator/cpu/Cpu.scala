package org.akaii.s4gb.emulator.cpu

import org.akaii.s4gb.emulator.MemoryMap

/**
 * Represents the Gameboy CPU
 */
case class Cpu(state: Cpu.State)

object Cpu {
  case class State(
    registers: Registers,
    memory: MemoryMap,
    private var imeFlag: IMEFlag = IMEEnabled,
    private var executionMode: ExecutionMode = Running,
    private var elapsed: Int = 0
  ) {
    def setIME(value: Boolean): Unit = (value, imeFlag) match {
      case (true, IMEDisabled) => imeFlag = IMEEnabling
      case (false, _) => imeFlag = IMEDisabled
      case _ => ()
    }

    def imeEnabled: Boolean = imeFlag.enabled
    def changeExecutionMode(newMode: ExecutionMode): Unit = { executionMode = newMode }
    def setElapsed(cycles: Int): Unit = { elapsed = cycles }
    def getElapsed: Int = elapsed
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
  case object Running extends ExecutionMode
  case object Halted extends ExecutionMode
  case object Stopped extends ExecutionMode
}