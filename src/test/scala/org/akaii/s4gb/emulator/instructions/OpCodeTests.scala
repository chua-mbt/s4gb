package org.akaii.s4gb.emulator.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.instructions.OpCode.Extract.*
import spire.math.UByte

class OpCodeTests extends FunSuite {
  test("range extraction") {
    val testCases = Seq(
      // Format: (byte, (hi, lo), expected)
      (0x00.toUByte, (5, 4), 0),
      (0x10.toUByte, (5, 4), 1),  // 00010000 -> bits 5-4 = 01
      (0x20.toUByte, (5, 4), 2),  // 00100000 -> bits 5-4 = 10

      (0x00.toUByte, (3, 2), 0),
      (0x04.toUByte, (3, 2), 1),  // 00000100 -> bits 3-2 = 01
      (0x08.toUByte, (3, 2), 2),  // 00001000 -> bits 3-2 = 10

      (0x00.toUByte, (1, 0), 0),
      (0x01.toUByte, (1, 0), 1),  // 00000001 -> bits 1-0 = 01
      (0x02.toUByte, (1, 0), 2),  // 00000010 -> bits 1-0 = 10

      (0x00.toUByte, (5, 3), 0),
      (0x08.toUByte, (5, 3), 1),  // 00001000 -> bits 5-3 = 001
      (0x10.toUByte, (5, 3), 2),  // 00010000 -> bits 5-3 = 010
      (0x20.toUByte, (5, 3), 4),  // 00100000 -> bits 5-3 = 100

      (0x00.toUByte, (2, 0), 0),
      (0x01.toUByte, (2, 0), 1),  // 00000001 -> bits 2-0 = 001
      (0x02.toUByte, (2, 0), 2),  // 00000010 -> bits 2-0 = 010
      (0x04.toUByte, (2, 0), 4)   // 00000100 -> bits 2-0 = 100
    )

    testCases.foreach { case (byte, (hi, lo), expected) =>
      val actual = byte.range(hi, lo)
      assertEquals(actual, expected)
    }
  }
}
