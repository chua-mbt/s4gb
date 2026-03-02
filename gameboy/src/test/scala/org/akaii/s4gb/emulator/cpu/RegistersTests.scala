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

      assertEquals(regs(r16.hi), value.hiByte)
      assertEquals(regs(r16.lo), value.loByte)
    }
  }

  test("CPU flag keeps lower nibble clear") {
    val regs = Registers()

    regs.f = 0xFF.toUByte
    assertEquals(regs.f, 0xF0.toUByte)

    regs.f = 0x0F.toUByte
    assertEquals(regs.f, 0x00.toUByte)

    regs.f = 0xAA.toUByte
    assertEquals(regs.f, 0xA0.toUByte)
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

  test("AF accessor round-trip and synchronization with A and F") {
    val regs = Registers()
    regs.af = 0x12FF.toUShort
    assertEquals(regs.a, 0x12.toUByte)
    assertEquals(regs.f, 0xF0.toUByte)
    assertEquals(regs.af, 0x12F0.toUShort)

    regs.af = 0x34A0.toUShort
    assertEquals(regs.a, 0x34.toUByte)
    assertEquals(regs.f, 0xA0.toUByte)
    assertEquals(regs.af, 0x34A0.toUShort)

    regs.a = 0x56.toUByte
    assertEquals(regs.af, 0x56A0.toUShort)

    regs.f = 0x0F.toUByte
    assertEquals(regs.f, 0x00.toUByte)
    assertEquals(regs.af, 0x5600.toUShort)

    regs.f = 0xB3.toUByte
    assertEquals(regs.f, 0xB0.toUByte)
    assertEquals(regs.af, 0x56B0.toUShort)

    regs.flags.clear()
    assertEquals(regs.af.loByte, 0x00.toUByte)
    regs.flags.z = true
    assertEquals(regs.af.loByte, 0x80.toUByte)
    regs.flags.z = false
    assertEquals(regs.af.loByte, 0x00.toUByte)
  }

  test("SP access and manipulation") {
    val regs = Registers()
    assertEquals(regs.sp, 0x0000.toUShort)

    regs.sp = 0x1234.toUShort
    assertEquals(regs.sp, 0x1234.toUShort)

    regs.advanceSP(1)
    assertEquals(regs.sp, 0x1235.toUShort)

    regs.advanceSP(0x0A)
    assertEquals(regs.sp, 0x123F.toUShort)

    regs.sp = 0xFFFF.toUShort
    regs.advanceSP(1)
    assertEquals(regs.sp, 0x0000.toUShort)

    regs.sp = 0xABCD.toUShort
    regs.updateSPHi(0x12.toUByte)
    assertEquals(regs.sp, 0x12CD.toUShort)

    regs.updateSPLo(0x34.toUByte)
    assertEquals(regs.sp, 0x1234.toUShort)

    regs.sp = 0xFFFF.toUShort
    regs.updateSPHi(0x00.toUByte)
    assertEquals(regs.sp, 0x00FF.toUShort)

    regs.updateSPLo(0x00.toUByte)
    assertEquals(regs.sp, 0x0000.toUShort)

    regs.sp = 0x0000.toUShort
    regs.updateSPHi(0xDE.toUByte)
    regs.updateSPLo(0xAD.toUByte)
    assertEquals(regs.sp, 0xDEAD.toUShort)
  }

  test("PC access and manipulation") {
    val regs = Registers()

    assertEquals(regs.pc, 0x0000.toUShort)

    regs.pc = 0x1234.toUShort
    assertEquals(regs.pc, 0x1234.toUShort)

    regs.advancePC(1)
    assertEquals(regs.pc, 0x1235.toUShort)

    regs.advancePC(0x0A)
    assertEquals(regs.pc, 0x123F.toUShort)

    regs.pc = 0xFFFF.toUShort
    regs.advancePC(1)
    assertEquals(regs.pc, 0x0000.toUShort)

    regs.pc = 0xABCD.toUShort
    regs.updatePCHi(0x12.toUByte)
    assertEquals(regs.pc, 0x12CD.toUShort)

    regs.updatePCLo(0x34.toUByte)
    assertEquals(regs.pc, 0x1234.toUShort)

    regs.pc = 0xFFFF.toUShort
    regs.updatePCHi(0x00.toUByte)
    assertEquals(regs.pc, 0x00FF.toUShort)

    regs.updatePCLo(0x00.toUByte)
    assertEquals(regs.pc, 0x0000.toUShort)

    regs.pc = 0x0000.toUShort
    regs.updatePCHi(0xDE.toUByte)
    regs.updatePCLo(0xAD.toUByte)
    assertEquals(regs.pc, 0xDEAD.toUShort)
  }
}
