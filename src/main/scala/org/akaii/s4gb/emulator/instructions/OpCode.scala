package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.instructions.OpCode.Masks.*
import spire.math.UByte
import spire.syntax.literals.*

/**
 * Represents an instruction in the Gameboy instruction set.
 *
 * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
 */
enum OpCode(val pattern: UByte, val mask: UByte = 0xFF.toUByte) {
  // Block 0: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
  case NOP extends OpCode(0x00.toUByte) // 00000000

  case LD_R16_IMM16 extends OpCode(0x01.toUByte, excludeBits54) // 00DD0001
  case LD_R16MEM_A extends OpCode(0x02.toUByte, excludeBits54) // 00DD0010
  case LD_A_R16MEM extends OpCode(0x0A.toUByte, excludeBits54) // 00SS1010
  case LD_MEM_IMM16_SP extends OpCode(0x08.toUByte) // 00001000

  case INC_R16 extends OpCode(0x03.toUByte, excludeBits54) // 00OO0011
  case DEC_R16 extends OpCode(0x0B.toUByte, excludeBits54) // 00OO1011
  case ADD_HL_R16 extends OpCode(0x09.toUByte, excludeBits54) // 00OO1001

  case INC_R8 extends OpCode(0x04.toUByte, excludeBits543) // 00OOO100
  case DEC_R8 extends OpCode(0x05.toUByte, excludeBits543) // 00OOO101

  case LD_R8_IMM8 extends OpCode(0x06.toUByte, excludeBits543) // 00OOO110
}

object OpCode {
  def decode(byteValue: UByte): OpCode = {
    // mask out opcode parameters, and then compare with pattern
    values.find(op => (byteValue & op.mask) == op.pattern)
      .getOrElse(throw new RuntimeException(f"Unknown opcode: ${byteValue.toHexString}"))
  }

  object Masks {
    val excludeBits54: UByte = 0xCF.toUByte // 11001111
    val excludeBits543: UByte = 0xC7.toUByte // 11000111
  }

  object Extract {
    def bits54(value: UByte): Int = ((value >>> 4) & 0x03.toUByte).toInt

    def bits543(value: UByte): Int = ((value >>> 3) & 0x07.toUByte).toInt
  }
}