package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.MemoryMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import spire.math.{UByte, UShort}

/**
 * Represents an instruction in the Gameboy instruction set.
 *
 * Raw input should always be a triple of bytes (up to 3 bytes per instruction).
 *
 * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
 */
sealed abstract class Instruction(protected val value: Array[UByte]) extends Product with Serializable {
  val opCode: UByte = value.head

  def execute(registers: Registers, memory: MemoryMap): Unit = ???

  override def toString: String = f"$productPrefix(0x${opCode.toInt}%02X)"
}

object Instruction {
  def decode(input: Array[UByte]): Instruction = {
    OpCode.decode(input.head) match {
      // Block 0 (0b00)  https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
      case OpCode.NOP => NOP
      case OpCode.LD_R16_IMM16 => LD_R16_IMM16(input)
      case OpCode.LD_R16MEM_A => LD_R16MEM_A(input)
      case OpCode.LD_A_R16MEM => LD_A_R16MEM(input)
      case OpCode.LD_MEM_IMM16_SP => LD_MEM_IMM16_SP(input)
      case OpCode.INC_R16 => INC_R16(input)
      case OpCode.DEC_R16 => DEC_R16(input)
      case OpCode.ADD_HL_R16 => ADD_HL_R16(input)
      case OpCode.INC_R8 => INC_R8(input)
      case OpCode.DEC_R8 => DEC_R8(input)
      case OpCode.LD_R8_IMM8 => LD_R8_IMM8(input)
      // Block 1 (0b01) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-1-8-bit-register-to-register-loads
      // Block 2 (0b10) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-2-8-bit-arithmetic
      // Block 3 (0b11) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-3
      // TODO: implement other instructions
    }
  }

  trait HasImm8 {
    self: Instruction =>
    val imm8: UByte = value(1)

    override def toString: String = {
      val hexStr = f"${self.opCode.toInt}%02X" + f"${imm8.toInt}%02X"
      s"$productPrefix(0x$hexStr)"
    }
  }

  trait HasImm16 {
    self: Instruction =>
    val imm16: UShort = (value(2).toUShort << 8) | value(1).toUShort

    override def toString: String = {
      val hexStr = f"${self.opCode.toInt}%02X" +
        f"${self.value(1).toInt}%02X" +
        f"${self.value(2).toInt}%02X"
      s"$productPrefix(0x$hexStr)"
    }
  }

  trait Has54Operand {
    self: Instruction =>
    private val operand54: Int = OpCode.Extract.bits54(opCode)
    val operand: Registers.R16 = Registers.R16.values(operand54)
  }

  trait Has543Operand {
    self: Instruction =>
    private val operand543: Int = OpCode.Extract.bits543(opCode)
    val operand: Registers.R8 = Registers.R8.values(operand543)
  }

  /*
   * Load instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Load_instructions
   *
   * Generally, direction is from right to left (i.e., LD dest <- src)
   **/

  /**
   * LD_R16_IMM16 - Copy the value imm16 (n16) into register r16.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_r16,n16]]
   */
  case class LD_R16_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    lazy val dest: Registers.R16 = Registers.R16.values(OpCode.Extract.bits54(opCode))

    override def execute(registers: Registers, memory: MemoryMap): Unit = {
      registers.update(dest, imm16)
    }
  }

  /**
   * LD_R16MEM_A - Copy the value in register A into the byte pointed to by r16.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__r16_,A]]
   */
  case class LD_R16MEM_A(private val input: Array[UByte]) extends Instruction(input) {
    private val rawDestRef: Int = OpCode.Extract.bits54(opCode)
    val destRef: Registers.R16 = Registers.R16.values(rawDestRef)

    override def execute(registers: Registers, memory: MemoryMap): Unit = {
      memory.write(registers(destRef), registers.a)
    }
  }

  /**
   * LD_A_R16MEM - Copy the byte pointed to by r16 into register A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_A,_r16_]]
   */
  case class LD_A_R16MEM(private val input: Array[UByte]) extends Instruction(input) {
    private val rawSrcRef: Int = OpCode.Extract.bits54(opCode)
    val srcRef: Registers.R16 = Registers.R16.values(rawSrcRef)

    override def execute(registers: Registers, memory: MemoryMap): Unit = {
      registers.a = memory(registers(srcRef))
    }
  }

  /**
   * LD_R8_IMM8 - Copy the value n8 into register r8.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_r8,n8]]
   */
  case class LD_R8_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 {
    private val rawDest: Int = OpCode.Extract.bits543(opCode)
    val dest: Registers.R8 = Registers.R8.values(rawDest)

    override def execute(registers: Registers, memory: MemoryMap): Unit = {
      registers.update(dest, imm8)
    }
  }

  /*
   * 8-bit arithmetic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#8-bit_arithmetic_instructions
   **/

  case class INC_R8(private val input: Array[UByte]) extends Instruction(input) with Has543Operand

  case class DEC_R8(private val input: Array[UByte]) extends Instruction(input) with Has543Operand

  /*
   * 16-bit arithmetic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#16-bit_arithmetic_instructions
   **/

  case class ADD_HL_R16(private val input: Array[UByte]) extends Instruction(input) with Has54Operand

  case class INC_R16(private val input: Array[UByte]) extends Instruction(input) with Has54Operand

  case class DEC_R16(private val input: Array[UByte]) extends Instruction(input) with Has54Operand

  /*
   * Bitwise logic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Bitwise_logic_instructions
   **/

  /*
   * Bit flag instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Bit_flag_instructions
   **/

  /*
   * Bit shift instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Bit_shift_instructions
   **/

  /*
   * Jumps and subroutine instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Jumps_and_subroutine_instructions
   **/

  /*
   * Carry flag instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Carry_flag_instructions
   **/

  /*
   * Stack manipulation instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Stack_manipulation_instructions
   **/

  /**
   * LD_MEM_IMM16_SP - Copy SP & $FF at address n16 and SP >> 8 at address n16 + 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__n16_,SP]]
   */
  case class LD_MEM_IMM16_SP(private val input: Array[UByte]) extends Instruction(input) with HasImm16

  /*
   * Interrupt-related instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Interrupt-related_instructions
   **/

  /*
   * Miscellaneous instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Miscellaneous_instructions
   **/

  /**
   * NOP - No OPeration.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#NOP]]
   */
  case object NOP extends Instruction(0x0.toInstructionInput) {
    override def execute(registers: Registers, memory: MemoryMap): Unit = {}
  }

}


