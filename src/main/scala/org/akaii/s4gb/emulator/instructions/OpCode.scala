package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
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

  case RLCA extends OpCode(0x07.toUByte) // 00000111
  case RRCA extends OpCode(0x0F.toUByte) // 00001111

  // Block 1: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-1-8-bit-register-to-register-loads
  case HALT extends OpCode(0x76.toUByte, excludeNone) // 01110110
  case LD_R8_R8 extends OpCode(0x40.toUByte, excludeBits543210) // 01DDDSSS

  // Block 2: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-2-8-bit-arithmetic

  // Block 3: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-3
}

object OpCode {

  def decode(byteValue: UByte): OpCode = {
    // mask out opcode parameters, and then compare with pattern
    values.find(op => (byteValue & op.mask) == op.pattern)
      .getOrElse(throw new RuntimeException(f"Unknown opcode: ${byteValue.toHexString}"))
  }

  object Masks {
    val excludeNone: UByte = 0xFF.toUByte // 11111111
    val excludeBits54: UByte = 0xCF.toUByte // 11001111
    val excludeBits543: UByte = 0xC7.toUByte // 11000111
    val excludeBits543210: UByte = 0xC0.toUByte // 11000000
  }

  /**
   * Groups of OpCode Parameters
   * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
   */
  object Parameters {
    enum R8 {
      case B, C, D, E, H, L, MEM_HL, A

      def toRegister: Registers.R8 = this match {
        case B => Registers.R8.B
        case C => Registers.R8.C
        case D => Registers.R8.D
        case E => Registers.R8.E
        case H => Registers.R8.H
        case L => Registers.R8.L
        case MEM_HL => throw new UnsupportedOperationException("No mapping for MEM_HL to R8")
        case A => Registers.R8.A
      }
    }

    object R8 {
      val nonMemHLValues: Seq[R8] = Seq(B, C, D, E, H, L, A)
    }

    enum R16 {
      case BC, DE, HL, SP

      def toRegister: Registers.R16 = this match {
        case BC => Registers.R16.BC
        case DE => Registers.R16.DE
        case HL => Registers.R16.HL
        case SP => throw new UnsupportedOperationException("No mapping for SP to R16")
      }
    }

    object R16 {
      val nonSPValues: Seq[R16] = Seq(BC, DE, HL)
    }

    enum R16Stack {
      case BC, DE, HL, AF
    }

    enum R16Mem {
      case BC, DE, HLPlus, HLMinus
    }

    enum Condition {
      case NZ, Z, NC, C
    }
  }

  object Extract {
    extension (value: UByte)
      def range(hi: Int, lo: Int): Int = (value.toInt >>> lo) & ((1 << (hi - lo + 1)) - 1)
  }
}