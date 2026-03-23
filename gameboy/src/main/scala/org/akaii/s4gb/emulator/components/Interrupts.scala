package org.akaii.s4gb.emulator.components

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.memorymap.RegisterMap
import spire.math.{UByte, UShort}

import scala.collection.mutable

/**
 * Interrupt registers
 *
 * @see [[https://gbdev.io/pandocs/Interrupts.html]]
 */
case class Interrupts() extends RegisterMap {

  import Interrupts.Address.*
  import Interrupts.{Masks, Source}

  /**
   * Initializes these interrupts to DMG power-up state.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html#hardware-registers]]
   */
  def initialize(): Unit = {
    registers(INTERRUPT_FLAG) = UByte(0xE1)
    registers(INTERRUPT_ENABLE) = UByte(0x00)
  }

  def request(source: Source): Unit = {
    val mask = UByte(1 << source.bit)
    registers(INTERRUPT_FLAG) = registers(INTERRUPT_FLAG) | mask
  }

  def clear(source: Source): Unit = {
    val mask = UByte(1 << source.bit)
    registers(INTERRUPT_FLAG) = registers(INTERRUPT_FLAG) & ~mask
  }

  override protected val registers: mutable.Map[UShort, UByte] =
    mutable.Map(
      INTERRUPT_ENABLE -> UByte(0),
      INTERRUPT_FLAG -> UByte(0)
    )

  override def apply(address: UShort): UByte = {
    val byte = super.apply(address)
    if(address == INTERRUPT_FLAG) byte | Masks.IGNORED else byte
  }

  override def write(address: UShort, value: UByte): Unit = {
    if (address == INTERRUPT_FLAG) {
      registers(INTERRUPT_FLAG) = value & ~Masks.IGNORED
    } else {
      super.write(address, value)
    }
  }
}

object Interrupts {
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