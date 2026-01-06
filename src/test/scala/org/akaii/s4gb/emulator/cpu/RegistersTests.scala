package org.akaii.s4gb.emulator.cpu

import utest.*

object RegistersTests extends TestSuite {
  val tests: Tests = Tests {
    test("AF register - setting 16-bit value updates 8-bit components") {
      val regs = Registers()
      
      regs.af = 0x1234
      assert(regs.a == 0x12)
      assert(regs.f == 0x30) // Lower 4 bits of F are always 0
      
      assert(regs.af == 0x1230)
      
      regs.af = 0xABCD
      assert(regs.a == 0xAB)
      assert(regs.f == 0xC0) // Lower 4 bits masked
      assert(regs.af == 0xABC0)
    }

    test("BC register - setting 16-bit value updates 8-bit components") {
      val regs = Registers()
      
      regs.bc = 0x5678
      assert(regs.b == 0x56)
      assert(regs.c == 0x78)
      
      assert(regs.bc == 0x5678)
      
      regs.bc = 0xABCD
      assert(regs.b == 0xAB)
      assert(regs.c == 0xCD)
      assert(regs.bc == 0xABCD)
    }

    test("DE register - setting 16-bit value updates 8-bit components") {
      val regs = Registers()
      
      regs.de = 0x9ABC
      assert(regs.d == 0x9A)
      assert(regs.e == 0xBC)
      
      assert(regs.de == 0x9ABC)
      
      regs.de = 0x0011
      assert(regs.d == 0x00)
      assert(regs.e == 0x11)
      assert(regs.de == 0x0011)
    }

    test("HL register - setting 16-bit value updates 8-bit components") {
      val regs = Registers()
      
      regs.hl = 0xDEF0
      assert(regs.h == 0xDE)
      assert(regs.l == 0xF0)
      
      assert(regs.hl == 0xDEF0)
      
      regs.hl = 0xFF00
      assert(regs.h == 0xFF)
      assert(regs.l == 0x00)
      assert(regs.hl == 0xFF00)
    }

    test("Flag Z (Zero) - getter and setter") {
      val regs = Registers()

      assert(!regs.flags.z)

      regs.flags.z = true
      assert(regs.flags.z)
      assert((regs.f & 0x80) != 0)

      regs.flags.z = false
      assert(!regs.flags.z)
      assert((regs.f & 0x80) == 0)

      regs.f = 0xFF
      regs.flags.z = false
      assert(!regs.flags.z)
      assert((regs.f & 0x7F) == 0x7F) // Other flags should remain
    }

    test("Flag N (Subtraction) - getter and setter") {
      val regs = Registers()
      
      assert(!regs.flags.n)
      
      regs.flags.n = true
      assert(regs.flags.n)
      assert((regs.f & 0x40) != 0)
      
      regs.flags.n = false
      assert(!regs.flags.n)
      assert((regs.f & 0x40) == 0)
      
      regs.f = 0xFF
      regs.flags.n = false
      assert(!regs.flags.n)
      assert((regs.f & 0xBF) == 0xBF) // Other flags should remain
    }

    test("Flag H (Half Carry) - getter and setter") {
      val regs = Registers()
      
      assert(!regs.flags.h)
      
      regs.flags.h = true
      assert(regs.flags.h)
      assert((regs.f & 0x20) != 0)
      
      regs.flags.h = false
      assert(!regs.flags.h)
      assert((regs.f & 0x20) == 0)
      
      regs.f = 0xFF
      regs.flags.h = false
      assert(!regs.flags.h)
      assert((regs.f & 0xDF) == 0xDF) // Other flags should remain
    }

    test("Flag C (Carry) - getter and setter") {
      val regs = Registers()
      
      assert(!regs.flags.c)
      
      regs.flags.c = true
      assert(regs.flags.c)
      assert((regs.f & 0x10) != 0)
      
      regs.flags.c = false
      assert(!regs.flags.c)
      assert((regs.f & 0x10) == 0)
      
      regs.f = 0xFF
      regs.flags.c = false
      assert(!regs.flags.c)
      assert((regs.f & 0xEF) == 0xEF) // Other flags should remain
    }

    test("All flags can be set independently") {
      val regs = Registers()
      
      regs.flags.z = true
      regs.flags.n = true
      regs.flags.h = true
      regs.flags.c = true
      
      assert(regs.flags.z)
      assert(regs.flags.n)
      assert(regs.flags.h)
      assert(regs.flags.c)
      assert(regs.f == 0xF0) // All flag bits set (upper 4 bits)
      
      regs.flags.z = false
      regs.flags.n = false
      regs.flags.h = false
      regs.flags.c = false
      
      assert(!regs.flags.z)
      assert(!regs.flags.n)
      assert(!regs.flags.h)
      assert(!regs.flags.c)
      assert(regs.f == 0x00)
    }

    test("AF register sets flags from value (lower 4 bits of F are always 0)") {
      val regs = Registers()
      
      // Set all flags first
      regs.flags.z = true
      regs.flags.n = true
      regs.flags.h = true
      regs.flags.c = true
      
      // Set AF - this overwrites F register (lower 4 bits are masked to 0)
      regs.af = 0x1234
      assert(regs.a == 0x12)
      assert(regs.f == 0x30) // 0x34 & 0xF0 = 0x30 (lower 4 bits masked)
      // Flags are set from the F value: z=0, n=0, h=1, c=1
      assert(!regs.flags.z)
      assert(!regs.flags.n)
      assert(regs.flags.h)
      assert(regs.flags.c)
      
      // Test with flags set in the value
      regs.af = 0xABF0 // F = 0xF0 means all flags set
      assert(regs.a == 0xAB)
      assert(regs.f == 0xF0)
      assert(regs.flags.z)
      assert(regs.flags.n)
      assert(regs.flags.h)
      assert(regs.flags.c)
    }

    test("16-bit register round-trip consistency") {
      val regs = Registers()
      
      val testValues = Seq(0x0000, 0x0001, 0x00FF, 0x0100, 0x1234, 0xFFFF)
      
      for (value <- testValues) {
        regs.bc = value
        assert(regs.bc == value)
        
        regs.de = value
        assert(regs.de == value)
        
        regs.hl = value
        assert(regs.hl == value)
      }
      
      // AF is special - lower 4 bits are masked
      for (value <- testValues) {
        regs.af = value
        val expected = (value & 0xFFF0) // Lower 4 bits masked
        assert(regs.af == expected)
      }
    }
  }
}
