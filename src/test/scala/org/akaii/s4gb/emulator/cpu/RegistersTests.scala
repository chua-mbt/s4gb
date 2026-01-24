package org.akaii.s4gb.emulator.cpu

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers.*
import spire.math.{UByte, UShort}
import spire.syntax.literals.*

class RegistersTests extends FunSuite {
  test("8-bit registers round-trip") {
    val regs = Registers()

    val cases: Seq[(R8, UByte => Unit, () => UByte)] = Seq(
      (R8.A, regs.a_=, () => regs.a),
      (R8.B, regs.b_=, () => regs.b),
      (R8.C, regs.c_=, () => regs.c),
      (R8.D, regs.d_=, () => regs.d),
      (R8.E, regs.e_=, () => regs.e),
      (R8.H, regs.h_=, () => regs.h),
      (R8.L, regs.l_=, () => regs.l)
    )

    val values = Seq(0x00, 0x12, 0x7F, 0x80, 0xFF).map(UByte(_))
    cases.zip(values).foreach { case ((r8, set, get), value) =>
      set(value)
      assertEquals(get(), value)
    }
  }

  test("16-bit registers round-trip and 8-bit access") {
    val regs = Registers()

    val cases: Seq[(R16, UShort => Unit, () => UShort)] = Seq(
      (R16.BC, regs.bc_=, () => regs.bc),
      (R16.DE, regs.de_=, () => regs.de),
      (R16.HL, regs.hl_=, () => regs.hl)
    )

    val values = Seq(0x0000, 0x1234, 0x7FFF, 0x8000, 0xFFFF).map(UShort(_))

    cases.zip(values).foreach { case ((r16, set16, get16), value) =>
      set16(value)
      assertEquals(get16(), value)

      assertEquals(regs(r16.hi), value.registerHiByte)
      assertEquals(regs(r16.lo), value.registerLoByte)
    }
  }

  test("CPU flags round-trip using enum with mask") {
    val regs = Registers()

    val cases: Seq[(Flag, Boolean => Unit, () => Boolean)] = Seq(
      (Flag.Z, regs.flags.z_=, () => regs.flags.z),
      (Flag.N, regs.flags.n_=, () => regs.flags.n),
      (Flag.H, regs.flags.h_=, () => regs.flags.h),
      (Flag.C, regs.flags.c_=, () => regs.flags.c)
    )

    cases.foreach { case (flag, set, get) =>
      assert(!get())

      set(true)
      assert(get())
      assertNotEquals(regs.f & flag.mask, UByte(0))

      set(false)
      assert(!get())
      assertEquals(regs.f & flag.mask, UByte(0))
    }
  }
}
