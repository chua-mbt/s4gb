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
  var a: Int = 0,
  var f: Int = 0,
  var b: Int = 0,
  var c: Int = 0,
  var d: Int = 0,
  var e: Int = 0,
  var h: Int = 0,
  var l: Int = 0,
  var sp: Int = 0,
  var pc: Int = 0
) {

  def af: Int = (a << 8) | f
  def af_=(v: Int): Unit = { a = hi8(v); f = lo8(v) & 0xF0 }

  def bc: Int = (b << 8) | c
  def bc_=(v: Int): Unit = { val v16 = v & 0xFFFF; b = hi8(v16); c = lo8(v16) }

  def de: Int = (d << 8) | e
  def de_=(v: Int): Unit = { val v16 = v & 0xFFFF; d = hi8(v16); e = lo8(v16) }

  def hl: Int = (h << 8) | l
  def hl_=(v: Int): Unit = { val v16 = v & 0xFFFF; h = hi8(v16); l = lo8(v16) }

  // helpers
  @inline private def hi8(v: Int): Int = (v >>> 8) & 0xFF
  @inline private def lo8(v: Int): Int = v & 0xFF

  // flags
  object flags {
    /** Zero flag */
    @inline def z: Boolean = (f & 0x80) != 0
    @inline def z_=(v: Boolean): Unit = f = (f & ~0x80) | (if (v) 0x80 else 0)

    /** Subtraction flag (BCD) */
    @inline def n: Boolean = (f & 0x40) != 0
    @inline def n_=(v: Boolean): Unit = f = (f & ~0x40) | (if (v) 0x40 else 0)

    /** Half Carry flag (BCD) */
    @inline def h: Boolean = (f & 0x20) != 0
    @inline def h_=(v: Boolean): Unit = f = (f & ~0x20) | (if (v) 0x20 else 0)

    /** Carry flag */
    @inline def c: Boolean = (f & 0x10) != 0
    @inline def c_=(v: Boolean): Unit = f = (f & ~0x10) | (if (v) 0x10 else 0)
  }
}