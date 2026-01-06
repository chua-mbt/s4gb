package org.akaii.s4gb.emulator.cpu

/**
 * Represents an instruction in the Gameboy instruction set.
 *
 * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
 */
sealed abstract class Instruction(private val input: Int) {
  val value: Int = input & 0xFFFFFF // full instruction value (up to 3 bytes)
  val opCode: Int = input & 0xFF
  val imm8: Option[Int] = None
  val imm16: Option[Int] = None

  override def toString: String = s"[${value.toHexString}] : ${super.toString}"
}

trait HasImm8 {
  self: Instruction =>
  override val imm8: Option[Int] = Some((value >>> 8) & 0xFF)
}

trait HasImm16 {
  self: Instruction =>
  override val imm16: Option[Int] = {
    val low  = (value >>> 8) & 0xFF
    val high = (value >>> 16) & 0xFF
    Some(low | (high << 8)) // little-endian
  }
}

object Instruction {
  def decode(input: Int): Instruction = {
    val opCode = input & 0xFF
    OpCode.decode(opCode.toByte) match {
      case OpCode.NOP => NOP
      case OpCode.LD_R16_IMM16 => LD_R16_IMM16(input)
      case OpCode.LD_R16MEM_A => LD_R16MEM_A(input)
      case OpCode.LD_A_R16MEM => LD_A_R16MEM(input)
      case OpCode.LD_MEM_IMM16_SP => LD_MEM_IMM16_SP(input)
      // TODO: implement other instructions
    }
  }

  // Block 0 https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
  object NOP extends Instruction(0x0)

  case class LD_R16_IMM16(private val input: Int) extends Instruction(input) with HasImm16

  case class LD_R16MEM_A(private val input: Int) extends Instruction(input)

  case class LD_A_R16MEM(private val input: Int) extends Instruction(input)

  case class LD_MEM_IMM16_SP(private val input: Int) extends Instruction(input) with HasImm16
}


