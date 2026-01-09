package org.akaii.s4gb.emulator.cpu

import org.akaii.s4gb.emulator.byteops.*
import spire.math.{UByte, UShort}
import spire.syntax.literals.*

/**
 * The Game Boy CPU is based on the Z80 architecture and includes:
 * - 8-bit registers: A, B, C, D, E, H, L, and Flags
 * - 16-bit registers: AF, BC, DE, HL, SP (Stack Pointer), PC (Program Counter)
 * - Flag register with Zero (Z), Subtraction (N), Half Carry (H), and Carry (C) flags
 *
 * @see [[https://gbdev.io/pandocs/CPU_Registers_and_Flags.html CPU Registers and Flags]]
 */
case class Registers(
  private val underlying: Array[UByte] = Array.fill(8)(UByte.MinValue),
  var sp: UByte = UByte.MinValue,
  var pc: UByte = UByte.MinValue
) {

  import Registers.*

  private val byteMask: UShort = 0xFF.toUShort
  private val flagMask: UByte = 0xF0.toUByte

  def apply(r: R8): UByte = underlying(r.ordinal)
  def update(r: R8, v: UByte): Unit = underlying.update(r.ordinal, v)

  def apply(r: R16): UShort = (underlying(r.hi.ordinal).toUShort << 8) | underlying(r.lo.ordinal).toUShort
  def update(r: R16, v: UShort): Unit = {
    underlying.update(r.hi.ordinal, v.registerHiByte)
    underlying.update(r.lo.ordinal, v.registerLoByte)
  }

  /** 8-bit registers (direct) */
  def a: UByte = apply(R8.A)
  def a_=(v: UByte): Unit = update(R8.A, v)

  def b: UByte = apply(R8.B)
  def b_=(v: UByte): Unit = update(R8.B, v)

  def c: UByte = apply(R8.C)
  def c_=(v: UByte): Unit = update(R8.C, v)

  def d: UByte = apply(R8.D)
  def d_=(v: UByte): Unit = update(R8.D, v)

  def e: UByte = apply(R8.E)
  def e_=(v: UByte): Unit = update(R8.E, v)

  def h: UByte = apply(R8.H)
  def h_=(v: UByte): Unit = update(R8.H, v)

  def l: UByte = apply(R8.L)
  def l_=(v: UByte): Unit = update(R8.L, v)

  def f: UByte = apply(R8.F)
  def f_=(v: UByte): Unit = update(R8.F, v & flagMask)

  /** 16-bit registers (direct) */
  def bc: UShort = apply(R16.BC)
  def bc_=(v: UShort): Unit = update(R16.BC, v)

  def de: UShort = apply(R16.DE)
  def de_=(v: UShort): Unit = update(R16.DE, v)

  def hl: UShort = apply(R16.HL)
  def hl_=(v: UShort): Unit = update(R16.HL, v)

  def af: UShort = (a.toUShort << 8) | f.toUShort

  def af_=(v: UShort): Unit = {
    a = v.registerHiByte
    f = v.registerLoByte & flagMask
  }

  object flags {
    def apply(flag: Flag): Boolean = (f & flag.mask) != UByte(0)
    def update(flag: Flag, value: Boolean): Unit =
      f = (f & ~flag.mask) | (if value then flag.mask else UByte(0))

    @inline def z: Boolean = apply(Flag.Z)
    @inline def z_=(v: Boolean): Unit = update(Flag.Z, v)

    @inline def n: Boolean = apply(Flag.N)
    @inline def n_=(v: Boolean): Unit = update(Flag.N, v)

    @inline def h: Boolean = apply(Flag.H)
    @inline def h_=(v: Boolean): Unit = update(Flag.H, v)

    @inline def c: Boolean = apply(Flag.C)
    @inline def c_=(v: Boolean): Unit = update(Flag.C, v)
  }


}

object Registers {

  enum R8 {
    case B, C, D, E, H, L, F, A
  }

  enum R16(val hi: R8, val lo: R8) {
    case BC extends R16(R8.B, R8.C)
    case DE extends R16(R8.D, R8.E)
    case HL extends R16(R8.H, R8.L)
    case AF extends R16(R8.A, R8.F)
  }

  enum Flag(val mask: UByte) {
    case Z extends Flag(UByte(0x80))
    case N extends Flag(UByte(0x40))
    case H extends Flag(UByte(0x20))
    case C extends Flag(UByte(0x10))
  }

}
