package org.akaii.s4gb.emulator.cpu

import OpCode.Masks._

/**
 * Represents an instruction in the Gameboy instruction set.
 *
 * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
 */
enum OpCode(val pattern: Int, val mask: Int = 0xFF) {
  // Block 0: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
  case NOP extends OpCode(0x00) // 00000000

  case LD_R16_IMM16 extends OpCode(0x01, excludeBits54) // 00DD0001
  case LD_R16MEM_A extends OpCode(0x02, excludeBits54) // 00DD0010
  case LD_A_R16MEM extends OpCode(0x0A, excludeBits54) // 00SS1010
  case LD_MEM_IMM16_SP extends OpCode(0x08) // 00001000

  case INC_R16 extends OpCode(0x03, excludeBits54) // 00OO0011
  case DEC_R16 extends OpCode(0x0B, excludeBits54) // 00OO1011
  case ADD_HL_R16 extends OpCode(0x09, excludeBits54) // 00OO1001

  case INC_R8 extends OpCode(0x04, excludeBits543) // 00OOO100
  case DEC_R8 extends OpCode(0x05, excludeBits543) // 00OOO101

  case LD_R8_IMM8 extends OpCode(0x06, excludeBits543) // 00OOO110
}

object OpCode {
  def decode(byteValue: Byte): OpCode = {
    val unsigned = byteValue & 0xFF
    // mask out opcode parameters, and then compare with pattern
    values.find(op => (unsigned & op.mask) == op.pattern)
      .getOrElse(throw new RuntimeException(f"Unknown opcode: $unsigned%02X"))
  }

  object Masks {
    val excludeBits54 = 0xCF // 11001111
    val excludeBits543 = 0xC7 // 11000111
  }

  object Extract {
    def bits54(value: Int): Int = (value >>> 4) & 0x03

    def bits543(value: Int): Int = (value >>> 3) & 0x07
  }
}