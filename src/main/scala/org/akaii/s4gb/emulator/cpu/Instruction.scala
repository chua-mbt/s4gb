package org.akaii.s4gb.emulator.cpu

/**
 * Represents an instruction in the Gameboy instruction set.
 *
 * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
 */
sealed abstract class Instruction(private val input: Int) extends Product with Serializable {
  val value: Int = input & 0xFFFFFF // full instruction value (up to 3 bytes)
  val opCode: Int = input & 0xFF
  val imm8: Option[Int] = None
  val imm16: Option[Int] = None

  override def toString: String = {
    val b0 = (value & 0xFF)
    val b1 = (value >>> 8) & 0xFF
    val b2 = (value >>> 16) & 0xFF
    f"$productPrefix(0x$b0%02X$b1%02X$b2%02X)"
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
      case OpCode.INC_R16 => INC_R16(input)
      case OpCode.DEC_R16 => DEC_R16(input)
      case OpCode.ADD_HL_R16 => ADD_HL_R16(input)
      case OpCode.INC_R8 => INC_R8(input)
      case OpCode.DEC_R8 => DEC_R8(input)
      case OpCode.LD_R8_IMM8 => LD_R8_IMM8(input)
      // TODO: implement other instructions
    }
  }

  trait HasImm8 {
    self: Instruction =>
    override val imm8: Option[Int] = Some((value >>> 8) & 0xFF)
  }

  trait HasImm16 {
    self: Instruction =>
    override val imm16: Option[Int] = {
      val low = (value >>> 8) & 0xFF
      val high = (value >>> 16) & 0xFF
      Some(low | (high << 8)) // little-endian
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

  // Block 0 https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
  case object NOP extends Instruction(0x0)

  case class LD_R16_IMM16(private val input: Int) extends Instruction(input) with HasImm16 {
    private val rawDest: Int = OpCode.Extract.bits54(opCode)
    val dest: Registers.R16 = Registers.R16.values(rawDest)
  }

  case class LD_R16MEM_A(private val input: Int) extends Instruction(input) {
    private val rawDestRef: Int = OpCode.Extract.bits54(opCode)
    val destRef: Registers.R16 = Registers.R16.values(rawDestRef)
  }

  case class LD_A_R16MEM(private val input: Int) extends Instruction(input) {
    private val rawSrcRef: Int = OpCode.Extract.bits54(opCode)
    val srcRef: Registers.R16 = Registers.R16.values(rawSrcRef)
  }

  case class LD_MEM_IMM16_SP(private val input: Int) extends Instruction(input) with HasImm16

  case class INC_R16(private val input: Int) extends Instruction(input) with Has54Operand

  case class DEC_R16(private val input: Int) extends Instruction(input) with Has54Operand

  case class ADD_HL_R16(private val input: Int) extends Instruction(input) with Has54Operand

  case class INC_R8(private val input: Int) extends Instruction(input) with Has543Operand

  case class DEC_R8(private val input: Int) extends Instruction(input) with Has543Operand

  case class LD_R8_IMM8(private val input: Int) extends Instruction(input) with HasImm8 {
    private val rawDest: Int = OpCode.Extract.bits543(opCode)
    val dest: Registers.R8 = Registers.R8.values(rawDest)
  }
}


