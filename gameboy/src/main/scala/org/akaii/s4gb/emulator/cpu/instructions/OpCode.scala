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
trait OpCode {
  def pattern: UByte
  def mask: UByte
}

object OpCode {

  /**
   * Un-prefixed instructions. Variable size from 1-3 bytes. Divided into 4 blocks based on the 2 most significant bits.
   *
   * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html#cpu-instruction-set]]
   */
  enum Base(val pattern: UByte, val mask: UByte = 0xFF.toUByte) extends OpCode {
    // Block 0: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
    case NOP extends Base(0x00.toUByte) // 00000000

    case LD_SP_IMM16 extends Base(0x31.toUByte) // 00110001
    case LD_R16_IMM16 extends Base(0x01.toUByte, excludeBits54) // 00DD0001
    case LD_R16MEM_A extends Base(0x02.toUByte, excludeBits54) // 00DD0010
    case LD_A_R16MEM extends Base(0x0A.toUByte, excludeBits54) // 00SS1010
    case LD_MEM_IMM16_SP extends Base(0x08.toUByte) // 00001000

    case INC_SP extends Base(0x33.toUByte) // 00110011
    case INC_R16 extends Base(0x03.toUByte, excludeBits54) // 00OO0011
    case DEC_SP extends Base(0x3B.toUByte) // 00111011
    case DEC_R16 extends Base(0x0B.toUByte, excludeBits54) // 00OO1011
    case ADD_HL_SP extends Base(0x39.toUByte) // 00001001
    case ADD_HL_R16 extends Base(0x09.toUByte, excludeBits54) // 00OO1001

    case INC_MEM_HL extends Base(0x34.toUByte) // 00110100
    case INC_R8 extends Base(0x04.toUByte, excludeBits543) // 00OOO100
    case DEC_MEM_HL extends Base(0x35.toUByte) // 00110101
    case DEC_R8 extends Base(0x05.toUByte, excludeBits543) // 00OOO101

    case LD_MEM_HL_IMM8 extends Base(0x36.toUByte) // 00110110
    case LD_R8_IMM8 extends Base(0x06.toUByte, excludeBits543) // 00OOO110

    case RLCA extends Base(0x07.toUByte) // 00000111
    case RRCA extends Base(0x0F.toUByte) // 00001111
    case RLA extends Base(0x17.toUByte) // 00010111
    case RRA extends Base(0x1F.toUByte) // 00011111
    case DAA extends Base(0x27.toUByte) // 00100111
    case CPL extends Base(0x2F.toUByte) // 00101111
    case SCF extends Base(0x37.toUByte) // 00110111
    case CCF extends Base(0x3F.toUByte) // 00111111

    case JR_IMM8 extends Base(0x18.toUByte) // 00011000
    case JR_COND_IMM8 extends Base(0x20.toUByte, excludeBits43) // 001CC000
    case STOP extends Base(0x10.toUByte) // 00010000

    // Block 1: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-1-8-bit-register-to-register-loads
    case HALT extends Base(0x76.toUByte, excludeNone) // 01110110
    case LD_MEM_HL_R8 extends Base(0x70.toUByte, excludeBits210) // 01110SSS
    case LD_R8_MEM_HL extends Base(0x46.toUByte, excludeBits543) // 01DDD110
    case LD_R8_R8 extends Base(0x40.toUByte, excludeBits543210) // 01DDDSSS

    // Block 2: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-2-8-bit-arithmetic
    case ADD_A_R8 extends Base(0x80.toUByte, excludeBits210) // 10000SSS
    case ADC_A_R8 extends Base(0x88.toUByte, excludeBits210) // 10001SSS
    case SUB_A_R8 extends Base(0x90.toUByte, excludeBits210) // 10010SSS
    case SBC_A_R8 extends Base(0x98.toUByte, excludeBits210) // 10011SSS
    case AND_A_MEM_HL extends Base(0xA6.toUByte) // 10100110
    case AND_A_R8 extends Base(0xA0.toUByte, excludeBits210) // 10100SSS
    case XOR_A_MEM_HL extends Base(0xAE.toUByte) // 10101110
    case XOR_A_R8 extends Base(0xA8.toUByte, excludeBits210) // 10101SSS
    case OR_A_MEM_HL extends Base(0xB6.toUByte) // 10110110
    case OR_A_R8 extends Base(0xB0.toUByte, excludeBits210) // 10110SSS
    case CP_A_R8 extends Base(0xB8.toUByte, excludeBits210) // 10111SSS

    // Block 3: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-3
    case ADD_A_IMM8 extends Base(0xC6.toUByte) // 11000110
    case ADC_A_IMM8 extends Base(0xCE.toUByte) // 11001110
    case SUB_A_IMM8 extends Base(0xD6.toUByte) // 11010110
    case SBC_A_IMM8 extends Base(0xDE.toUByte) // 11011110
    case AND_A_IMM8 extends Base(0xE6.toUByte) // 11100110
    case XOR_A_IMM8 extends Base(0xEE.toUByte) // 11101110
    case OR_A_IMM8 extends Base(0xF6.toUByte) // 11110110
    case CP_A_IMM8 extends Base(0xFE.toUByte) // 11111110

    case RET_COND extends Base(0xC0.toUByte, excludeBits43) // 110CC000
    case RET extends Base(0xC9.toUByte) // 11001001
    case RETI extends Base(0xD9.toUByte) // 11011001
    case JP_COND_IMM16 extends Base(0xC2.toUByte, excludeBits43) // 110CC010
    case JP_IMM16 extends Base(0xC3.toUByte) // 11000011
    case JP_HL extends Base(0xE9.toUByte) // 11101001
    case CALL_COND_IMM16 extends Base(0xC4.toUByte, excludeBits43) // 110CC100
    case CALL_IMM16 extends Base(0xCD.toUByte) // 11001101
    case RST_TGT3 extends Base(0xC7.toUByte, excludeBits543) // 11TTT111

    case POP_R16STK extends Base(0xC1.toUByte, excludeBits54) // 11RR0001
    case PUSH_R16STK extends Base(0xC5.toUByte, excludeBits54) // 11RR0101

    case LDH_MEM_C_A extends Base(0xE2.toUByte) // 11100010
    case LDH_MEM_IMM8_A extends Base(0xE0.toUByte) // 11100000
    case LD_MEM_IMM16_A extends Base(0xEA.toUByte) // 11101010
    case LDH_A_MEM_C extends Base(0xF2.toUByte) // 11110010
    case LDH_A_MEM_IMM8 extends Base(0xF0.toUByte) // 11110000
    case LD_A_MEM_IMM16 extends Base(0xFA.toUByte) // 11111010

    case ADD_SP_IMM8 extends Base(0xE8.toUByte) // 11101000
    case LD_HL_ADD_SP_IMM8 extends Base(0xF8.toUByte) // 11111000
    case LD_SP_HL extends Base(0xF9.toUByte) // 11111011

    case DI extends Base(0xF3.toUByte) // 11110011
    case EI extends Base(0xFB.toUByte) // 11111011
  }

  /**
   * 16-bit extended instructions prefixed by 0xCB. Require 2 fetches rather than 1.
   *
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#cb-prefix]]
   * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html#cb-prefix-instructions]]
   */
  enum CB(val pattern: UByte, val mask: UByte = 0xFF.toUByte) extends OpCode {
    case RLC_MEM_HL extends CB(0x06.toUByte) // 00000110
    case RLC_R8 extends CB(0x00.toUByte, excludeBits210) // 00000OOO
    case RRC_MEM_HL extends CB(0x0E.toUByte) // 00001110
    case RRC_R8 extends CB(0x08.toUByte, excludeBits210) // 00001OOO
    case RL_MEM_HL extends CB(0x16.toUByte) // 00010110
    case RL_R8 extends CB(0x10.toUByte, excludeBits210) // 00010OOO
    case RR_MEM_HL extends CB(0x1E.toUByte) // 00011110
    case RR_R8 extends CB(0x18.toUByte, excludeBits210) // 00011OOO
    case SLA_MEM_HL extends CB(0x26.toUByte) // 00100110
    case SLA_R8 extends CB(0x20.toUByte, excludeBits210) // 00100OOO
    case SRA_MEM_HL extends CB(0x2E.toUByte) // 00101110
    case SRA_R8 extends CB(0x28.toUByte, excludeBits210) // 00101OOO
  }

  /**
   * The following opcodes are invalid, and hard-lock the CPU until the console is powered off:
   * $D3, $DB, $DD, $E3, $E4, $EB, $EC, $ED, $F4, $FC, and $FD.
   *
   * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
   */
  val HOLES: Seq[UByte] =
    Seq(0xD3.toUByte, 0xDB.toUByte, 0xDD.toUByte, 0xE3.toUByte, 0xE4.toUByte, 0xEB.toUByte, 0xEC.toUByte,
      0xED.toUByte, 0xF4.toUByte, 0xFC.toUByte, 0xFD.toUByte)

  val PREFIXED: UByte = 0xCB.toUByte

  def decode[T <: OpCode](values: Array[T], byteValue: UByte): T = {
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
  trait Parameters {
    def ordinal: Int
  }

  object Parameters {
    enum R8 extends Parameters {
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

    enum R16 extends Parameters {
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

    enum R16Stack extends Parameters {
      case BC, DE, HL, AF

      def toRegister: Registers.R16 = this match {
        case BC => Registers.R16.BC
        case DE => Registers.R16.DE
        case HL => Registers.R16.HL
        case AF => throw new UnsupportedOperationException("No mapping for AF to R16")
      }
    }

    enum R16Mem extends Parameters {
      case BC, DE, HLPlus, HLMinus

      def toRegister: Registers.R16 = this match {
        case BC => Registers.R16.BC
        case DE => Registers.R16.DE
        case _ => Registers.R16.HL
      }
    }

    enum Condition extends Parameters {
      case NZ, Z, NC, C
    }
  }

  object Extract {
    extension (value: UByte)
      def range(hi: Int, lo: Int): Int = (value.toInt >>> lo) & ((1 << (hi - lo + 1)) - 1)
  }
}