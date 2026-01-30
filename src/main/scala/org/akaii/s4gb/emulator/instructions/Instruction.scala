package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.MemoryMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.instructions.OpCode.Extract.*
import spire.math.{UByte, UShort}

/**
 * Represents an instruction in the Gameboy instruction set.
 *
 * Raw input should always be a triple of bytes (up to 3 bytes per instruction).
 *
 * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
 * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7]]
 * @see [[https://izik1.github.io/gbops/]]
 */
sealed abstract class Instruction(protected val value: Array[UByte]) extends Product with Serializable {
  val opCode: UByte = value.head
  val cycles: Int
  val bytes: Int

  def execute(state: Instruction.State): Boolean = {
    if (state.elapsed < micro.length) {
      val microInstruction = micro(state.elapsed)
      microInstruction.execute(state)
      state.registers.advance(
        cycles = 1,
        bytes = if (state.elapsed == 0) bytes else 0
      )
    }

    state.elapsed + 1 > micro.length
  }

  protected[instructions] def micro: Seq[Instruction.Micro] = Seq(Instruction.Micro.fetchOpCode())

  protected def executeImplementation(state: Instruction.State): Unit = ???

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
      case OpCode.RLCA => RLCA
      case OpCode.RRCA => RRCA
      case OpCode.RLA => RLA
      case OpCode.RRA => RRA
      case OpCode.DAA => DAA
      case OpCode.CPL => CPL
      case OpCode.SCF => SCF
      case OpCode.CCF => CCF
      // Block 1 (0b01) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-1-8-bit-register-to-register-loads
      case OpCode.HALT => HALT
      case OpCode.LD_R8_R8 => LD_R8_R8(input)
      // Block 2 (0b10) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-2-8-bit-arithmetic
      // Block 3 (0b11) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-3
      // TODO: implement other instructions
    }
  }

  case class State(registers: Registers, memory: MemoryMap, elapsed: Int = 0)

  private type MicroInstruction = State => Unit

  /** Each MicroInstruction costs 1 cycle */
  final case class Micro private(execute: MicroInstruction = _ => ())

  /**
   * Types are distinguished only to understand how the cycle is being spent
   *
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595?utm_source=chatgpt.com#fetch-and-stuff]]
   * */
  object Micro {
    def readMemory(execute: MicroInstruction = _ => ()): Micro = Micro(execute)

    def writeMemory(execute: MicroInstruction = _ => ()): Micro = Micro(execute)

    def fetchOpCode(execute: MicroInstruction = _ => ()): Micro = readMemory(execute)

    def iduOperation(execute: MicroInstruction = _ => ()): Micro = Micro(execute)
  }

  trait HasImm8 {
    self: Instruction =>
    lazy val imm8: UByte = value(1)

    override def toString: String = {
      val hexStr = f"${self.opCode.toInt}%02X" + f"${imm8.toInt}%02X"
      s"$productPrefix(0x$hexStr)"
    }
  }

  trait HasImm16 {
    self: Instruction =>
    lazy val imm16: UShort = (value(2).toUShort << 8) | value(1).toUShort

    override def toString: String = {
      val hexStr = f"${self.opCode.toInt}%02X" +
        f"${self.value(1).toInt}%02X" +
        f"${self.value(2).toInt}%02X"
      s"$productPrefix(0x$hexStr)"
    }
  }

  trait HasR8Operand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.R8 = OpCode.Parameters.R8.values(opCode.range(start, start - 2))

    protected def operandContents(operandStart: Int, state: State): UByte =
      operand(operandStart) match {
        case OpCode.Parameters.R8.MEM_HL =>
          state.memory(state.registers.hl)
        case parameter =>
          state.registers(parameter.toRegister)
      }

    protected def writeToOperandLocation(operandStart: Int, state: State, value: UByte): Unit =
      operand(operandStart) match {
        case OpCode.Parameters.R8.MEM_HL =>
          state.memory.write(state.registers.hl, value)
        case parameter =>
          state.registers.update(parameter.toRegister, value)
      }
  }

  trait HasR16Operand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.R16 = OpCode.Parameters.R16.values(opCode.range(start, start - 1))

    protected def operandContents(operandStart: Int, state: State): UShort =
      operand(operandStart) match {
        case OpCode.Parameters.R16.SP =>
          state.registers.sp
        case parameter =>
          state.registers(parameter.toRegister)
      }

    protected def writeToOperandLocation(operandStart: Int, state: State, value: UShort): Unit =
      operand(operandStart) match {
        case OpCode.Parameters.R16.SP =>
          state.registers.sp = value
        case parameter =>
          state.registers.update(parameter.toRegister, value)
      }

    protected def writeToOperandHiLocation(operandStart: Int, state: State, value: UShort): Unit =
      operand(operandStart) match {
        case OpCode.Parameters.R16.SP =>
          state.registers.updateSPHi(value.registerHiByte)
        case parameter =>
          state.registers.update(parameter.toRegister.hi, value.registerHiByte)
      }

    protected def writeToOperandLoLocation(operandStart: Int, state: State, value: UShort): Unit =
      operand(operandStart) match {
        case OpCode.Parameters.R16.SP =>
          state.registers.updateSPLo(value.registerLoByte)
        case parameter =>
          state.registers.update(parameter.toRegister.lo, value.registerLoByte)
      }
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
  case class LD_R16_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand with HasImm16 {
    override val cycles: Int = 3
    override val bytes: Int = 3

    private val destStart = 5
    lazy val dest: OpCode.Parameters.R16 = operand(destStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchOpCode { state => writeToOperandLoLocation(destStart, state, imm16) },
      Micro.fetchOpCode { state => writeToOperandHiLocation(destStart, state, imm16) },
    )
  }

  /**
   * LD_R16MEM_A - Copy the value in register A into the byte pointed to by r16.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__r16_,A]]
   */
  case class LD_R16MEM_A(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand {
    override val cycles: Int = 2
    override val bytes: Int = 1

    private val destRefStart = 5
    lazy val destRef: OpCode.Parameters.R16 = operand(destRefStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.writeMemory { state => state.memory.write(operandContents(destRefStart, state), state.registers.a) }
    )
  }

  /**
   * LD_A_R16MEM - Copy the byte pointed to by r16 into register A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_A,_r16_]]
   */
  case class LD_A_R16MEM(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand {
    override val cycles: Int = 2
    override val bytes: Int = 1

    private val srcRefStart = 5
    lazy val srcRef: OpCode.Parameters.R16 = operand(srcRefStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state => state.registers.a = state.memory(operandContents(srcRefStart, state)) }
    )
  }

  /**
   * LD_R8_IMM8 - Copy the value n8 into register r8.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_r8,n8]]
   */
  case class LD_R8_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with HasImm8 {
    override val cycles: Int = 2
    override val bytes: Int = 2

    private val destStart = 5
    lazy val dest: OpCode.Parameters.R8 = operand(destStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state => writeToOperandLocation(destStart, state, imm8) }
    )
  }

  /**
   * LD_R8_R8 - Copy (aka Load) the value in register on the right into the register on the left.
   *
   * Storing a register into itself is a no-op; however, some Game Boy emulators interpret LD B,B as a breakpoint,
   * or LD D,D as a debug message (such as BGB).
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_r8,r8]]
   */
  case class LD_R8_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand {
    override val cycles: Int = 1
    override val bytes: Int = 1

    private val srcStart = 2
    lazy val src: OpCode.Parameters.R8 = operand(srcStart)

    private val destStart = 5
    lazy val dest: OpCode.Parameters.R8 = operand(destStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => writeToOperandLocation(destStart, state, operandContents(srcStart, state)) }
    )
  }

  /*
   * 8-bit arithmetic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#8-bit_arithmetic_instructions
   **/

  /**
   * INC_R8 - Increment the value in register r8 by 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#INC_r8]]
   */
  case class INC_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand {
    override val cycles: Int = 1
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val originalValue = operandContents(operandStart, state)
        val result = originalValue + 1.toUByte
        writeToOperandLocation(operandStart, state, result)
        state.registers.flags.z = result == 0.toUByte
        state.registers.flags.n = false
        state.registers.flags.h = (originalValue & 0x0F.toUByte) + 1.toUByte > 0x0F.toUByte // Just the bottom nibble
      }
    )
  }

  /**
   * DEC_R8 - Decrement the value in register r8 by 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DEC_r8]]
   */
  case class DEC_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand {
    override val cycles: Int = 1
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val originalValue = operandContents(operandStart, state)
        val result = originalValue - 1.toUByte
        writeToOperandLocation(operandStart, state, result)
        state.registers.flags.z = result == 0.toUByte
        state.registers.flags.n = true
        state.registers.flags.h = (originalValue & 0x0F.toUByte) < 1.toUByte // Just the bottom nibble
      }
    )
  }

  /*
   * 16-bit arithmetic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#16-bit_arithmetic_instructions
   **/

  /**
   * ADD_HL_R16 - Add the value in r16 to HL.
   *
   * Because the ALU is 8bit, it needs two 8bit adds to add the two 16bit numbers together.
   * First cycle is low 8bit add, 2nd cycle is high 8bit add with fetch happening in parallel.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADD_HL,r16]]
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595?utm_source=chatgpt.com#add-hl-r16]]
   */
  case class ADD_HL_R16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand {
    override val cycles: Int = 2
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R16 = operand(operandStart)

    private var carryFromLow: UByte = UByte.MinValue
    override protected[instructions] def micro: Seq[Micro] = Seq(
        Micro.iduOperation { state =>
          val hl = state.registers.hl
          val op = operandContents(operandStart, state)
          val lSum = hl.registerLoByte.toInt + op.registerLoByte.toInt
          carryFromLow = if(lSum > UByte.MaxValue.toInt) 1.toUByte else 0.toUByte
          val halfCarryMask = 0x0FFF.toUShort
          state.registers.l = lSum.toUByte
          state.registers.flags.h = ((hl & halfCarryMask) + (op & halfCarryMask)) > halfCarryMask
        },
      Micro.iduOperation { state =>
        val hSum = state.registers.hl.registerHiByte.toInt +
          operandContents(operandStart, state).registerHiByte.toInt +
          carryFromLow.toInt

        state.registers.h = hSum.toUByte
        state.registers.flags.c = hSum > UByte.MaxValue.toInt
        state.registers.flags.n = false
      }
    )
  }

  /**
   * INC_R16 - Increment the value in register r16 by 1.
   *
   * The ALU is actually not used at all for this one! This is just IDU magic.
   * 16bit register is output to IDU, set to either increment or decrement, and a writeback is issued.
   * Because fetch also uses the IDU to post-increment PC, the beforementioned use of the IDU causes a cycle penalty,
   * and so the instruction takes two cycles to execute, as only one IDU operation can execute per M-cycle.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#INC_r16]]
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595?utm_source=chatgpt.com#incdec-r16]]
   */
  case class INC_R16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand {
    override val cycles: Int = 2
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R16 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state =>
        val original = operandContents(operandStart, state)
        val result = original + 1.toUShort
        writeToOperandLocation(operandStart, state, result)
      }
    )
  }

  /**
   * DEC_R16 - Decrement the value in register r16 by 1.
   *
   * See INC_R16 for why this takes 2 cycles.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DEC_r16]]
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595?utm_source=chatgpt.com#incdec-r16]]
   */
  case class DEC_R16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand {
    override val cycles: Int = 2
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R16 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state =>
        val original = operandContents(operandStart, state)
        val result = original - 1.toUShort
        writeToOperandLocation(operandStart, state, result)
      }
    )
  }

  /*
   * Bitwise logic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Bitwise_logic_instructions
   **/
  /**
   * CPL - ComPLement accumulator (A = ~A); also called bitwise NOT.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#CPL]]
   */
  case object CPL extends Instruction(Array(OpCode.CPL.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        state.registers.a = ~state.registers.a
        state.registers.flags.n = true
        state.registers.flags.h = true
      }
    )
  }

  /*
   * Bit flag instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Bit_flag_instructions
   **/

  /*
   * Bit shift instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Bit_shift_instructions
   **/

  /**
   * RLCA - Rotate register A left.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RLCA]]
   */
  case object RLCA extends Instruction(Array(OpCode.RLCA.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carry = (a & 0x80.toUByte) >> 7
        val result = (a << 1) | carry
        state.registers.a = result
        state.registers.flags.z = false
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = carry == 1.toUByte
      }
    )
  }

  /**
   * RRCA - Rotate register A right.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RRCA]]
   */
  case object RRCA extends Instruction(Array(OpCode.RRCA.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carry = a & 0x01.toUByte
        val result = (a >> 1) | (carry << 7)
        state.registers.a = result
        state.registers.flags.z = false
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = carry == 1.toUByte
      }
    )
  }

  /**
   * RLA - Rotate register A left, through the carry flag.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RLA]]
   */
  case object RLA extends Instruction(Array(OpCode.RLA.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carryIn = if (state.registers.flags.c) 1.toUByte else 0.toUByte
        val carryOut = (a & 0x80.toUByte) >> 7
        val result = (a << 1) | carryIn
        state.registers.a = result
        state.registers.flags.z = false
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = carryOut == 1.toUByte
      }
    )
  }

  /**
   * RRA - Rotate register A right, through the carry flag.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RRA]]
   */
  case object RRA extends Instruction(Array(OpCode.RRA.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carryIn = if (state.registers.flags.c) 0x80.toUByte else 0.toUByte
        val carryOut = a & 0x01.toUByte
        val result = (a >> 1) | carryIn
        state.registers.a = result
        state.registers.flags.z = false
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = carryOut == 1.toUByte
      }
    )
  }

  /*
   * Jumps and subroutine instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Jumps_and_subroutine_instructions
   **/

  /*
   * Carry flag instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Carry_flag_instructions
   **/
  /**
   * SCF - Set Carry Flag.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#SCF]]
   */
  case object SCF extends Instruction(Array(OpCode.SCF.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = true
      }
    )
  }

  case object CCF extends Instruction(Array(OpCode.CCF.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = !state.registers.flags.c
      }
    )
  }

  /*
   * Stack manipulation instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Stack_manipulation_instructions
   **/

  /**
   * LD_MEM_IMM16_SP - Copy SP & $FF at address n16 and SP >> 8 at address n16 + 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__n16_,SP]]
   */
  case class LD_MEM_IMM16_SP(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    override val cycles: Int = 5
    override val bytes: Int = 3

    private var originalSP: UShort = UShort.MinValue
    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => originalSP = state.registers.sp },
      Micro.fetchOpCode(),
      Micro.fetchOpCode(),
      Micro.writeMemory { state => state.memory.write(imm16, originalSP.registerLoByte) },
      Micro.writeMemory { state => state.memory.write(imm16 + 1.toUShort, originalSP.registerHiByte) }
    )
  }

  /*
   * Interrupt-related instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Interrupt-related_instructions
   **/

  /**
   * Halt - Enter CPU low-power consumption mode until an interrupt occurs.
   *
   * The exact behavior of this instruction depends on the state of the IME flag, and whether interrupts are
   * pending (i.e. whether ‘[IE] & [IF]’ is non-zero):
   *
   * If the IME flag is set:
   * The CPU enters low-power mode until after an interrupt is about to be serviced. The handler is executed
   * normally, and the CPU resumes execution after the HALT when that returns.
   *
   * If the IME flag is not set, and no interrupts are pending:
   * As soon as an interrupt becomes pending, the CPU resumes execution. This is like the above, except that
   * the handler is not called.
   *
   * If the IME flag is not set, and some interrupt is pending:
   * The CPU continues execution after the HALT, but the byte after it is read twice in a row
   * (PC is not incremented, due to a hardware bug).
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#HALT]]
   */
  case object HALT extends Instruction(Array(OpCode.HALT.pattern)) {
    override val cycles: Int = 1 // TODO
    override val bytes: Int = 1 // TODO

    override def executeImplementation(state: Instruction.State): Unit = ??? // TODO
  }

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
    override val cycles: Int = 1
    override val bytes: Int = 1

    override def executeImplementation(state: Instruction.State): Unit = {}
  }

  /**
   * DAA - Decimal Adjust Accumulator.
   *
   * Designed to be used after performing an arithmetic instruction (ADD, ADC, SUB, SBC) whose inputs were in
   * Binary-Coded Decimal (BCD), adjusting the result to likewise be in BCD.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DAA]]
   */
  case object DAA extends Instruction(Array(OpCode.DAA.pattern)) {
    override val cycles: Int = 1
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        var adjustment = 0
        if(state.registers.flags.n) {
          if(state.registers.flags.h) adjustment += 0x06
          if(state.registers.flags.c) adjustment += 0x60
          adjustment *= -1
        } else {
          if(state.registers.flags.h || (state.registers.a & 0x0F.toUByte) > 0x09.toUByte) adjustment += 0x06
          if(state.registers.flags.c || state.registers.a > 0x99.toUByte) {
            adjustment += 0x60
            state.registers.flags.c = true
          }
        }

        val adjustedA = (state.registers.a.toInt + adjustment).toUByte
        state.registers.a = adjustedA
        state.registers.flags.z = adjustedA == 0.toUByte
        state.registers.flags.h = false
      }
    )
  }
}


