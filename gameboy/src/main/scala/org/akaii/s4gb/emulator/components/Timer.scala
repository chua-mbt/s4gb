package org.akaii.s4gb.emulator.components

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.memorymap.{MemoryMap, RegisterMap}
import spire.math.{UByte, UShort}

import scala.collection.mutable

/**
 * Timer and Divider Registers
 *
 * @see [[https://gbdev.io/pandocs/Timer_and_Divider_Registers.html]]
 * @see [[https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html#timer-obscure-behaviour]]
 */
case class Timer(interrupts: Interrupts) extends RegisterMap {

  import Timer.*
  import Timer.Address.*

  /**
   * System counter
   *
   * DIV is just the visible part of the system counter.
   * The system counter is constantly incrementing every M-cycle, unless the CPU is in STOP mode.
   */
  private var systemCounter: UShort = UShort(0)

  override protected val registers: mutable.Map[UShort, UByte] = mutable.Map(
    TIMA -> UByte(0),
    TMA -> UByte(0),
    TAC -> UByte(0),
  )

  def tick(): Unit = {
    val previousCounter = systemCounter
    systemCounter += 1.toUShort
    incrementTIMA(previousCounter, systemCounter)
  }

  override def apply(address: UShort): UByte = address match {
    case DIV => systemCounter.hiByte
    case other => super.apply(other)
  }

  override def write(address: UShort, value: UByte): Unit = address match {
    case DIV => systemCounter = UShort(0)
    case other => super.write(other, value)
  }

  @inline private def isTimerEnabled: Boolean = (registers(TAC) & Masks.TAC_ENABLE) != UByte(0)

  @inline private def atFrequencyBoundary(previous: UShort, current: UShort): Boolean = {
    val watchedBit = watchedBitForTAC(registers(TAC))
    ((previous >>> watchedBit) & UShort(1)) == UShort(1) && ((current >>> watchedBit) & UShort(1)) == UShort(0)
  }

  @inline private def incrementTIMA(previous: UShort, current: UShort): Unit =
    if (isTimerEnabled && atFrequencyBoundary(previous, current)) {
      if (registers(TIMA) == UByte(0xFF)) {
        registers(TIMA) = registers(TMA)
        interrupts.request(Interrupts.Source.Timer)
      } else {
        registers(TIMA) = registers(TIMA) + UByte(1)
      }
    }

  private[components] def setSystemCounter(value: UShort): Unit = systemCounter = value
}

object Timer {
  object Address {
    /**
     * FF04 — DIV: Divider register
     *
     * @see [[https://gbdev.io/pandocs/Timer_and_Divider_Registers.html#ff04--div-divider-register]]
     */
    val DIV: UShort = UShort(0xFF04)

    /**
     * FF05 — TIMA: Timer counter
     *
     * @see [[https://gbdev.io/pandocs/Timer_and_Divider_Registers.html#ff05--tima-timer-counter]]
     */
    val TIMA: UShort = UShort(0xFF05)

    /**
     * FF06 — TMA: Timer modulo
     *
     * @see [[https://gbdev.io/pandocs/Timer_and_Divider_Registers.html#ff06--tma-timer-modulo]]
     */
    val TMA: UShort = UShort(0xFF06)

    /**
     * FF07 — TAC: Timer control
     *
     * @see [[https://gbdev.io/pandocs/Timer_and_Divider_Registers.html#ff07--tac-timer-control]]
     */
    val TAC: UShort = UShort(0xFF07)

    /**
     * Timer start address
     */
    val TIMER_START: UShort = DIV

    /**
     * Timer end address (inclusive)
     */
    val TIMER_END: UShort = TAC
  }

  object Masks {
    /**
     * Enable: Controls whether TIMA is incremented. Note that DIV is always counting, regardless of this bit.
     *
     * @see [[https://gbdev.io/pandocs/Timer_and_Divider_Registers.html#ff07--tac-timer-control]]
     */
    val TAC_ENABLE: UByte = UByte(0x04)

    /**
     * Clock select: Controls the frequency at which TIMA is incremented, as follows:
     *
     * @see [[https://gbdev.io/pandocs/Timer_and_Divider_Registers.html#ff07--tac-timer-control]]
     */
    val TAC_CLOCK_SELECT: UByte = UByte(0x03)
  }

  /**
   * CPU = 4_194_304 (t-cycle frequency)
   *
   * | TAC bits | Frequency (Hz) | Calculation                | systemCounter bit (n) |
   * |----------|----------------|----------------------------|-----------------------|
   * | 0x00     | 4096           | CPU / 2^(9+1) = 4096 Hz    | 9                     |
   * | 0x01     | 262_144        | CPU / 2^(3+1) = 262_144 Hz | 3                     |
   * | 0x02     | 65_536         | CPU / 2^(5+1) = 65_536 Hz  | 5                     |
   * | 0x03     | 16_384         | CPU / 2^(7+1) = 16_384 Hz  | 7                     |
   */
  private object CounterBit {
    val for4096: Int = 9
    val for262_144: Int = 3
    val for65_536: Int = 5
    val for16_384: Int = 7
  }

  @inline private[components] def watchedBitForTAC(tac: UByte): Int = (tac & Masks.TAC_CLOCK_SELECT).toInt match {
    case 0x00 => CounterBit.for4096
    case 0x01 => CounterBit.for262_144
    case 0x02 => CounterBit.for65_536
    case 0x03 => CounterBit.for16_384
  }
}
