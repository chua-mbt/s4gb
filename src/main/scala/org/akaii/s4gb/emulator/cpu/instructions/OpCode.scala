package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import OpCode.Masks.*
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

  case LD_SP_IMM16 extends OpCode(0x31.toUByte) // 00110001
  case LD_R16_IMM16 extends OpCode(0x01.toUByte, excludeBits54) // 00DD0001
  case LD_R16MEM_A extends OpCode(0x02.toUByte, excludeBits54) // 00DD0010
  case LD_A_R16MEM extends OpCode(0x0A.toUByte, excludeBits54) // 00SS1010
  case LD_MEM_IMM16_SP extends OpCode(0x08.toUByte) // 00001000

  case INC_SP extends OpCode(0x33.toUByte) // 00110011
  case INC_R16 extends OpCode(0x03.toUByte, excludeBits54) // 00OO0011
  case DEC_SP extends OpCode(0x3B.toUByte) // 00111011
  case DEC_R16 extends OpCode(0x0B.toUByte, excludeBits54) // 00OO1011
  case ADD_HL_SP extends OpCode(0x39.toUByte) // 00001001
  case ADD_HL_R16 extends OpCode(0x09.toUByte, excludeBits54) // 00OO1001

  case INC_MEM_HL extends OpCode(0x34.toUByte) // 00110100
  case INC_R8 extends OpCode(0x04.toUByte, excludeBits543) // 00OOO100
  case DEC_MEM_HL extends OpCode(0x35.toUByte) // 00110101
  case DEC_R8 extends OpCode(0x05.toUByte, excludeBits543) // 00OOO101

  case LD_MEM_HL_IMM8 extends OpCode(0x36.toUByte) // 00110110
  case LD_R8_IMM8 extends OpCode(0x06.toUByte, excludeBits543) // 00OOO110

  case RLCA extends OpCode(0x07.toUByte) // 00000111
  case RRCA extends OpCode(0x0F.toUByte) // 00001111
  case RLA extends OpCode(0x17.toUByte) // 00010111
  case RRA extends OpCode(0x1F.toUByte) // 00011111
  case DAA extends OpCode(0x27.toUByte) // 00100111
  case CPL extends OpCode(0x2F.toUByte) // 00101111
  case SCF extends OpCode(0x37.toUByte) // 00110111
  case CCF extends OpCode(0x3F.toUByte) // 00111111

  case JR_IMM8 extends OpCode(0x18.toUByte) // 00011000
  case JR_COND_IMM8 extends OpCode(0x20.toUByte, excludeBits43) // 001CC000
  // STOP

  // Block 1: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-1-8-bit-register-to-register-loads
  case HALT extends OpCode(0x76.toUByte, excludeNone) // 01110110
  case LD_MEM_HL_R8 extends OpCode(0x70.toUByte, excludeBits210) // 01110SSS
  case LD_R8_MEM_HL extends OpCode(0x46.toUByte, excludeBits543) // 01DDD110
  case LD_R8_R8 extends OpCode(0x40.toUByte, excludeBits543210) // 01DDDSSS

  // Block 2: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-2-8-bit-arithmetic
  case ADD_A_R8 extends OpCode(0x80.toUByte, excludeBits210) // 10000SSS
  case ADC_A_R8 extends OpCode(0x88.toUByte, excludeBits210) // 10001SSS
  case SUB_A_R8 extends OpCode(0x90.toUByte, excludeBits210) // 10010SSS
  case SBC_A_R8 extends OpCode(0x98.toUByte, excludeBits210) // 10011SSS
  case AND_A_MEM_HL extends OpCode(0xA6.toUByte) // 10100110
  case AND_A_R8 extends OpCode(0xA0.toUByte, excludeBits210) // 10100SSS
  case XOR_A_MEM_HL extends OpCode(0xAE.toUByte) // 10101110
  case XOR_A_R8 extends OpCode(0xA8.toUByte, excludeBits210) // 10101SSS
  case OR_A_MEM_HL extends OpCode(0xB6.toUByte) // 10110110
  case OR_A_R8 extends OpCode(0xB0.toUByte, excludeBits210) // 10110SSS
  case CP_A_R8 extends OpCode(0xB8.toUByte, excludeBits210) // 10111SSS

  // Block 3: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-3
  case ADD_A_IMM8 extends OpCode(0xC6.toUByte) // 11000110
  case ADC_A_IMM8 extends OpCode(0xCE.toUByte) // 11001110
  case SUB_A_IMM8 extends OpCode(0xD6.toUByte) // 11010110
  case SBC_A_IMM8 extends OpCode(0xDE.toUByte) // 11011110
  case AND_A_IMM8 extends OpCode(0xE6.toUByte) // 11100110
  case XOR_A_IMM8 extends OpCode(0xEE.toUByte) // 11101110
  case OR_A_IMM8 extends OpCode(0xF6.toUByte) // 11110110
  case CP_A_IMM8 extends OpCode(0xFE.toUByte) // 11111110

  case DI extends OpCode(0xF3.toUByte) // 11110011
  case EI extends OpCode(0xFB.toUByte) // 11111011
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
    val excludeBits43: UByte = 0xE7.toUByte // 11100111
    val excludeBits543: UByte = 0xC7.toUByte // 11000111
    val excludeBits210: UByte = 0xF8.toUByte // 11111000
    val excludeBits543210: UByte = 0xC0.toUByte // 11000000
  }

  /**
   * Groups of OpCode Parameters
   *
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

      def toRegister: Registers.R16 = this match {
        case BC => Registers.R16.BC
        case DE => Registers.R16.DE
        case _ => Registers.R16.HL
      }
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