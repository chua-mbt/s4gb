package org.akaii.s4gb.emulator

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
  import Interrupts.Masks
  import Interrupts.Source

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
  enum Source(val bit: Int) {
    case VBlank extends Source(0)
    case LCDStat extends Source(1)
    case Timer extends Source(2)
    case Serial extends Source(3)
    case Joypad extends Source(4)
  }
}