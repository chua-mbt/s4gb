package org.akaii.s4gb.emulator.components

import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import org.akaii.s4gb.extensions.byteops.*
import spire.math.{UByte, UShort}

/**
 * Interrupt registers
 *
 * @see [[https://gbdev.io/pandocs/Interrupts.html]]
 */
class Interrupts extends MemoryMap {

  import Interrupts.Address.*
  import Interrupts.{Masks, Source}

  private var interruptFlag: UByte = UByte(0)
  private var interruptEnable: UByte = UByte(0)

  /**
   * Initializes these interrupts to DMG power-up state.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html#hardware-registers]]
   */
  def initialize(): Unit = {
    interruptFlag = UByte(0xE1)
    interruptEnable = UByte(0x00)
  }

  def request(source: Source): Unit = {
    val mask = UByte(1 << source.bit)
    interruptFlag = interruptFlag | mask
  }

  def clear(source: Source): Unit = {
    val mask = UByte(1 << source.bit)
    interruptFlag = interruptFlag & ~mask
  }

  override def apply(address: UShort): UByte =
    if (address == INTERRUPT_FLAG) interruptFlag | Masks.IGNORED
    else if (address == INTERRUPT_ENABLE) interruptEnable
    else throw new IllegalArgumentException(s"Invalid register address: $address")

  override def write(address: UShort, value: UByte): Unit =
    if (address == INTERRUPT_FLAG) interruptFlag = value & ~Masks.IGNORED
    else if (address == INTERRUPT_ENABLE) interruptEnable = value
    else throw new IllegalArgumentException(s"Invalid register address: $address")

  override def toString: String = {
    val entries = Seq(INTERRUPT_ENABLE -> interruptEnable, INTERRUPT_FLAG -> interruptFlag)
      .filter(_._2 != UByte(0))
      .sortBy(_._1.toInt)
      .map { case (address, value) => f"0x${address.toInt}%04X=0x${value.toInt}%02X" }
      .mkString(",")
    s"${getClass.getSimpleName}($entries)"
  }
}

object Interrupts {

  def apply(): Interrupts = new Interrupts

  object Address {
    /**
     * Interrupt Enable (IE).
     *
     * @see [[https://gbdev.io/pandocs/Interrupts.html#ffff--ie-interrupt-enable]]
     */
    val INTERRUPT_ENABLE: UShort = UShort(0xFFFF)

    /**
     * Interrupt Flag (IF).
     *
     * @see [[https://gbdev.io/pandocs/Interrupts.html#ff0f--if-interrupt-flag]]
     */
    val INTERRUPT_FLAG: UShort = UShort(0xFF0F)
  }

  object Masks {
    val IGNORED: UByte = UByte(0xE0)
  }

  /**
   * Position corresponding to a bit in the IF and IE registers.
   *
   * @see [[https://gbdev.io/pandocs/Interrupt_Sources.html]]
   */
  enum Source(val bit: Int, val handler: UShort) {
    case VBlank extends Source(0, 0x0040.toUShort)
    case LCDStat extends Source(1, 0x0048.toUShort)
    case Timer extends Source(2, 0x0050.toUShort)
    case Serial extends Source(3, 0x0058.toUShort)
    case Joypad extends Source(4, 0x0060.toUShort)
  }

  object Source {
    def highestPriority(ifRegisterValue: UByte): Source =
      Source.values
        .filter(s => ((ifRegisterValue >> s.bit) & 1.toUByte) != 0.toUByte)
        .minBy(_.bit)
  }
}