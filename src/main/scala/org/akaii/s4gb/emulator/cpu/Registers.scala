package org.akaii.s4gb.emulator.cpu

/**
 * The Game Boy CPU is based on the Z80 architecture and includes:
 * - 8-bit registers: A, B, C, D, E, H, L, and Flags
 * - 16-bit registers: AF, BC, DE, HL, SP (Stack Pointer), PC (Program Counter)
 * - Flag register with Zero (Z), Subtraction (N), Half Carry (H), and Carry (C) flags
 *
 * @see [[https://gbdev.io/pandocs/CPU_Registers_and_Flags.html CPU Registers and Flags]]
 */
case class Registers(
  private val underlying: Array[Int] = Array.fill(8)(0),
  var sp: Int = 0,
  var pc: Int = 0
) {

  import Registers._

  def apply(r: R8): Int = underlying(r.ordinal)
  def update(r: R8, value: Int): Unit = underlying(r.ordinal) = value & 0xFF

  def apply(r: R16): Int = (underlying(r.hi.ordinal) << 8) | underlying(r.lo.ordinal)
  def update(r: R16, value: Int): Unit = {
    val v = value & 0xFFFF
    underlying(r.hi.ordinal) = (v >>> 8) & 0xFF
    underlying(r.lo.ordinal) = v & 0xFF
  }

  /** 8-bit registers (direct) */
  def a: Int = underlying(R8.A.ordinal)
  def a_=(v: Int): Unit = underlying(R8.A.ordinal) = v & 0xFF

  def b: Int = underlying(R8.B.ordinal)
  def b_=(v: Int): Unit = underlying(R8.B.ordinal) = v & 0xFF

  def c: Int = underlying(R8.C.ordinal)
  def c_=(v: Int): Unit = underlying(R8.C.ordinal) = v & 0xFF

  def d: Int = underlying(R8.D.ordinal)
  def d_=(v: Int): Unit = underlying(R8.D.ordinal) = v & 0xFF

  def e: Int = underlying(R8.E.ordinal)
  def e_=(v: Int): Unit = underlying(R8.E.ordinal) = v & 0xFF

  def h: Int = underlying(R8.H.ordinal)
  def h_=(v: Int): Unit = underlying(R8.H.ordinal) = v & 0xFF

  def l: Int = underlying(R8.L.ordinal)
  def l_=(v: Int): Unit = underlying(R8.L.ordinal) = v & 0xFF

  def f: Int = underlying(R8.F.ordinal)
  def f_=(v: Int): Unit = underlying(R8.F.ordinal) = v & 0xFF // don't force lower 4 bits to 0 here

  /** 16-bit registers (direct) */
  def bc: Int = apply(R16.BC)
  def bc_=(v: Int): Unit = update(R16.BC, v)

  def de: Int = apply(R16.DE)
  def de_=(v: Int): Unit = update(R16.DE, v)

  def hl: Int = apply(R16.HL)
  def hl_=(v: Int): Unit = update(R16.HL, v)

  def af: Int = (a << 8) | f
  def af_=(v: Int): Unit = {
    a = (v >>> 8) & 0xFF; f = v & 0xF0
  }

  object flags {
    @inline def z: Boolean = (f & 0x80) != 0
    @inline def z_=(v: Boolean): Unit = f = (f & ~0x80) | (if v then 0x80 else 0)

    @inline def n: Boolean = (f & 0x40) != 0
    @inline def n_=(v: Boolean): Unit = f = (f & ~0x40) | (if v then 0x40 else 0)

    @inline def h: Boolean = (f & 0x20) != 0
    @inline def h_=(v: Boolean): Unit = f = (f & ~0x20) | (if v then 0x20 else 0)

    @inline def c: Boolean = (f & 0x10) != 0
    @inline def c_=(v: Boolean): Unit = f = (f & ~0x10) | (if v then 0x10 else 0)
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
    // SP is special and handled separately
  }

}
