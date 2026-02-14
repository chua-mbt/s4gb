package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.MemoryMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import OpCode.Extract.*
import org.akaii.s4gb.emulator.cpu.instructions.Instruction.Micro
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
  val cycles: Instruction.MCycle
  val bytes: Int

  def execute(state: Cpu.State): Boolean = {
    val earlyCompletion: Boolean =
      if (state.getMicroStep < micro.length) {
        val microInstruction = micro(state.getMicroStep)
        microInstruction.execute(state) == Micro.Done
      } else {
        false
      }

    earlyCompletion || (state.getElapsed + 1) > micro.length
  }

  protected[instructions] def micro: Seq[Instruction.Micro] = Seq(Instruction.Micro.fetchOpCode(bytes))

  protected def executeImplementation(state: Cpu.State): Unit = ???

  override def toString: String = f"$productPrefix(0x${opCode.toInt}%02X)"
}

object Instruction {
  def decode(input: Array[UByte]): Instruction = {
    OpCode.decode(input.head) match {
      // Block 0 (0b00)  https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
      case OpCode.NOP => NOP
      case OpCode.LD_SP_IMM16 => LD_SP_IMM16(input)
      case OpCode.LD_R16_IMM16 => LD_R16_IMM16(input)
      case OpCode.LD_R16MEM_A => LD_R16MEM_A(input)
      case OpCode.LD_A_R16MEM => LD_A_R16MEM(input)
      case OpCode.LD_MEM_IMM16_SP => LD_MEM_IMM16_SP(input)
      case OpCode.INC_SP => INC_SP
      case OpCode.INC_R16 => INC_R16(input)
      case OpCode.DEC_SP => DEC_SP
      case OpCode.DEC_R16 => DEC_R16(input)
      case OpCode.ADD_HL_SP => ADD_HL_SP
      case OpCode.ADD_HL_R16 => ADD_HL_R16(input)
      case OpCode.INC_MEM_HL => INC_MEM_HL
      case OpCode.INC_R8 => INC_R8(input)
      case OpCode.DEC_MEM_HL => DEC_MEM_HL
      case OpCode.DEC_R8 => DEC_R8(input)
      case OpCode.LD_MEM_HL_IMM8 => LD_MEM_HL_IMM8(input)
      case OpCode.LD_R8_IMM8 => LD_R8_IMM8(input)
      case OpCode.RLCA => RLCA
      case OpCode.RRCA => RRCA
      case OpCode.RLA => RLA
      case OpCode.RRA => RRA
      case OpCode.DAA => DAA
      case OpCode.CPL => CPL
      case OpCode.SCF => SCF
      case OpCode.CCF => CCF
      case OpCode.JR_IMM8 => JR_IMM8(input)
      case OpCode.JR_COND_IMM8 => JR_COND_IMM8(input)
      // Block 1 (0b01) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-1-8-bit-register-to-register-loads
      case OpCode.HALT => HALT
      case OpCode.LD_MEM_HL_R8 => LD_MEM_HL_R8(input)
      case OpCode.LD_R8_MEM_HL => LD_R8_MEM_HL(input)
      case OpCode.LD_R8_R8 => LD_R8_R8(input)
      // Block 2 (0b10) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-2-8-bit-arithmetic
      case OpCode.ADD_A_R8 => ADD_A_R8(input)
      case OpCode.ADC_A_R8 => ADC_A_R8(input)
      case OpCode.SUB_A_R8 => SUB_A_R8(input)
      case OpCode.SBC_A_R8 => SBC_A_R8(input)
      case OpCode.AND_A_MEM_HL => AND_A_MEM_HL
      case OpCode.AND_A_R8 => AND_A_R8(input)
      case OpCode.XOR_A_MEM_HL => XOR_A_MEM_HL
      case OpCode.XOR_A_R8 => XOR_A_R8(input)
      case OpCode.OR_A_MEM_HL => OR_A_MEM_HL
      case OpCode.OR_A_R8 => OR_A_R8(input)
      case OpCode.CP_A_R8 => CP_A_R8(input)
      // Block 3 (0b11) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-3
      case OpCode.ADD_A_IMM8 => ADD_A_IMM8(input)
      case OpCode.ADC_A_IMM8 => ADC_A_IMM8(input)
      case OpCode.SUB_A_IMM8 => SUB_A_IMM8(input)
      case OpCode.SBC_A_IMM8 => SBC_A_IMM8(input)
      case OpCode.AND_A_IMM8 => AND_A_IMM8(input)
      case OpCode.XOR_A_IMM8 => XOR_A_IMM8(input)
      case OpCode.OR_A_IMM8 => OR_A_IMM8(input)
      case OpCode.CP_A_IMM8 => CP_A_IMM8(input)
      case OpCode.DI => DI
      case OpCode.EI => EI
      // TODO: implement other instructions
    }
  }

  sealed trait MCycle {
    def withinCost(elapsed: Int): Boolean

    def maxCost: Int
  }

  object MCycle {
    case class Fixed(cost: Int) extends MCycle {
      override def withinCost(elapsed: Int): Boolean = elapsed == cost

      override def maxCost: Int = cost
    }

    case class Varying(costRange: Range) extends MCycle {
      override def withinCost(elapsed: Int): Boolean = costRange.contains(elapsed)

      override def maxCost: Int = costRange.max
    }
  }

  private type MicroStep = Cpu.State => Unit
  private type MicroGate = Cpu.State => Micro.Next

  /** Each MicroStep costs 1 cycle */
  final case class Micro private(execute: MicroGate)

  /**
   * Types are distinguished only to understand how the cycle is being spent
   *
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595?utm_source=chatgpt.com#fetch-and-stuff]]
   * */
  object Micro {
    sealed trait Next

    case object Done extends Next

    case object Continue extends Next

    private def advancePC(bytes: Int, state: Cpu.State): Unit = state.registers.advancePC(bytes)

    private def microOp(execute: MicroStep): Micro = Micro { state => execute(state); Continue }

    def fetchOpCode(bytes: Int): Micro = microOp { state => advancePC(bytes, state) }

    def fetchOpCode(bytes: Int)(execute: MicroStep = _ => ()): Micro = microOp { state =>
      advancePC(bytes, state); execute(state)
    }

    def readMemory(execute: MicroStep = _ => ()): Micro = microOp(execute)

    def readMemoryAndThen(execute: MicroGate): Micro = Micro { state => execute(state) }

    def writeMemory(execute: MicroStep = _ => ()): Micro = microOp(execute)

    def iduOperation(execute: MicroStep = _ => ()): Micro = microOp(execute)

    def aluOperation(execute: MicroStep = _ => ()): Micro = microOp(execute)

    def modifyPC(execute: MicroStep = _ => ()): Micro = microOp(execute)
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

    protected def operandContents(operandStart: Int, state: Cpu.State): UByte =
      operandContents(operand(operandStart), state)

    protected def operandContents(parameter: OpCode.Parameters.R8, state: Cpu.State): UByte =
      parameter match {
        case OpCode.Parameters.R8.MEM_HL =>
          state.memory(state.registers.hl)
        case parameter =>
          state.registers(parameter.toRegister)
      }

    protected def writeToOperandLocation(operandStart: Int, state: Cpu.State, value: UByte): Unit =
      writeToOperandLocation(operand(operandStart), state, value)

    protected def writeToOperandLocation(parameter: OpCode.Parameters.R8, state: Cpu.State, value: UByte): Unit =
      parameter match {
        case OpCode.Parameters.R8.MEM_HL =>
          state.memory.write(state.registers.hl, value)
        case parameter =>
          state.registers.update(parameter.toRegister, value)
      }
  }

  trait HasR16Operand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.R16 = OpCode.Parameters.R16.values(opCode.range(start, start - 1))

    protected def operandContents(operandStart: Int, state: Cpu.State): UShort =
      operandContents(operand(operandStart), state)

    protected def operandContents(operand: OpCode.Parameters.R16, state: Cpu.State): UShort =
      operand match {
        case OpCode.Parameters.R16.SP =>
          state.registers.sp
        case parameter =>
          state.registers(parameter.toRegister)
      }

    protected def writeToOperandLocation(operandStart: Int, state: Cpu.State, value: UShort): Unit =
      writeToOperandLocation(operand(operandStart), state, value)

    protected def writeToOperandLocation(operand: OpCode.Parameters.R16, state: Cpu.State, value: UShort): Unit =
      operand match {
        case OpCode.Parameters.R16.SP =>
          state.registers.sp = value
        case parameter =>
          state.registers.update(parameter.toRegister, value)
      }

    protected def writeToOperandHiLocation(operandStart: Int, state: Cpu.State, value: UShort): Unit =
      writeToOperandHiLocation(operand(operandStart), state, value)

    protected def writeToOperandHiLocation(operand: OpCode.Parameters.R16, state: Cpu.State, value: UShort): Unit =
      operand match {
        case OpCode.Parameters.R16.SP =>
          state.registers.updateSPHi(value.registerHiByte)
        case parameter =>
          state.registers.update(parameter.toRegister.hi, value.registerHiByte)
      }

    protected def writeToOperandLoLocation(operandStart: Int, state: Cpu.State, value: UShort): Unit =
      writeToOperandLoLocation(operand(operandStart), state, value)

    protected def writeToOperandLoLocation(operand: OpCode.Parameters.R16, state: Cpu.State, value: UShort): Unit =
      operand match {
        case OpCode.Parameters.R16.SP =>
          state.registers.updateSPLo(value.registerLoByte)
        case parameter =>
          state.registers.update(parameter.toRegister.lo, value.registerLoByte)
      }
  }

  trait HasCondOperand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.Condition = OpCode.Parameters.Condition.values(opCode.range(start, start - 1))

    def continueIf(condition: OpCode.Parameters.Condition)(state: Cpu.State): Micro.Next = {
      val shouldContinue = condition match {
        case OpCode.Parameters.Condition.NZ => !state.registers.flags.z
        case OpCode.Parameters.Condition.Z => state.registers.flags.z
        case OpCode.Parameters.Condition.NC => !state.registers.flags.c
        case OpCode.Parameters.Condition.C => state.registers.flags.c
      }
      if (shouldContinue) Micro.Continue else Micro.Done
    }
  }

  trait AddOperation {
    self: Instruction =>
    def resolve(state: Cpu.State, op1: UByte, op2: UByte, carryIn: UByte = 0.toUByte, storeInA: Boolean = false): Unit = {
      val sum = op1.toInt + op2.toInt + carryIn.toInt
      if (storeInA) state.registers.a = sum.toUByte
      state.registers.flags.z = sum.toUByte == 0.toUByte
      state.registers.flags.n = false
      state.registers.flags.h = op1.overflowFromBit3(op2, carryIn)
      state.registers.flags.c = op1.overflowFromBit7(op2, carryIn)
    }
  }

  trait SubOperation {
    self: Instruction =>
    def resolve(state: Cpu.State, op1: UByte, op2: UByte, carryIn: UByte = 0.toUByte, storeInA: Boolean = false): Unit = {
      val diff = op1.toInt - op2.toInt - carryIn.toInt
      if (storeInA) state.registers.a = diff.toUByte
      state.registers.flags.z = diff.toUByte == 0.toUByte
      state.registers.flags.n = true
      state.registers.flags.h = op1.borrowFromBit4(op2, carryIn)
      state.registers.flags.c = op1.borrowFrom(op2, carryIn)
    }
  }

  trait OrOperation {
    self: Instruction =>
    def resolve(state: Cpu.State, result: UByte): Unit = {
      state.registers.a = result
      state.registers.flags.z = result == 0.toUByte
      state.registers.flags.n = false
      state.registers.flags.h = false
      state.registers.flags.c = false
    }
  }

  trait AndOperation {
    self: Instruction =>
    def resolve(state: Cpu.State, result: UByte): Unit = {
      state.registers.a = result
      state.registers.flags.z = result == 0.toUByte
      state.registers.flags.n = false
      state.registers.flags.h = true
      state.registers.flags.c = false
    }
  }

  /*
   * Load instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Load_instructions
   *
   * Generally, direction is from right to left (i.e., LD dest <- src)
   **/

  /**
   * LD_SP_IMM16 - Copy the value imm16 (n16) into register SP.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_SP,n16]]
   */
  case class LD_SP_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 3

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state => writeToOperandHiLocation(OpCode.Parameters.R16.SP, state, imm16) },
      Micro.readMemory { state => writeToOperandLoLocation(OpCode.Parameters.R16.SP, state, imm16) }
    )
  }

  /**
   * LD_R16_IMM16 - Copy the value imm16 (n16) into register r16.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_r16,n16]]
   */
  case class LD_R16_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 3

    private val destStart = 5
    lazy val dest: OpCode.Parameters.R16 = operand(destStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state => writeToOperandLoLocation(destStart, state, imm16) },
      Micro.readMemory { state => writeToOperandHiLocation(destStart, state, imm16) },
    )
  }

  /**
   * LD_R16MEM_A - Copy the value in register A into the byte pointed to by r16.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__r16_,A]]
   */
  case class LD_R16MEM_A(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand {
    override val cycles: MCycle = MCycle.Fixed(2)
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
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private val srcRefStart = 5
    lazy val srcRef: OpCode.Parameters.R16 = operand(srcRefStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state => state.registers.a = state.memory(operandContents(srcRefStart, state)) }
    )
  }

  /**
   * LD_MEM_HL_IMM8 - Copy the value imm8 (n8) into the byte pointed to by HL.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__HL_,r8]]
   */
  case class LD_MEM_HL_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with HasR8Operand {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory(),
      Micro.writeMemory { state => writeToOperandLocation(OpCode.Parameters.R8.MEM_HL, state, imm8) }
    )
  }

  /**
   * LD_R8_IMM8 - Copy the value imm8 (n8) into register r8.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_r8,n8]]
   */
  case class LD_R8_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with HasImm8 {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    private val destStart = 5
    lazy val dest: OpCode.Parameters.R8 = operand(destStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state => writeToOperandLocation(destStart, state, imm8) }
    )
  }

  /**
   * LD_MEM_HL_R8 - Copy the value in register r8 into the byte pointed to by HL.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__HL_,r8]]
   */
  case class LD_MEM_HL_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private val srcStart = 2
    lazy val src: OpCode.Parameters.R8 = operand(srcStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.writeMemory { state => state.memory.write(state.registers.hl, operandContents(srcStart, state)) }
    )
  }

  /**
   * LD_R8_MEM_HL - Copy the value pointed to by HL into register r8.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__HL_,r8]]
   */
  case class LD_R8_MEM_HL(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private val destStart = 5
    lazy val dest: OpCode.Parameters.R8 = operand(destStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state => writeToOperandLocation(destStart, state, state.memory(state.registers.hl)) }
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val srcStart = 2
    lazy val src: OpCode.Parameters.R8 = operand(srcStart)

    private val destStart = 5
    lazy val dest: OpCode.Parameters.R8 = operand(destStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state => writeToOperandLocation(destStart, state, operandContents(srcStart, state)) }
    )
  }

  /*
   * 8-bit arithmetic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#8-bit_arithmetic_instructions
   **/

  /**
   * INC_MEM_HL - Increment the byte pointed to by HL by 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#INC__HL_]]
   */
  case object INC_MEM_HL extends Instruction(Array(OpCode.INC_MEM_HL.pattern)) with HasR8Operand {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory(),
      Micro.writeMemory { state =>
        val originalValue = state.memory(state.registers.hl)
        val result = originalValue + 1.toUByte
        state.memory.write(state.registers.hl, result)
        state.registers.flags.z = result == 0.toUByte
        state.registers.flags.n = false
        state.registers.flags.h = originalValue.overflowFromBit3(1.toUByte)
      }
    )
  }

  /**
   * INC_R8 - Increment the value in register r8 by 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#INC_r8]]
   */
  case class INC_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val originalValue = operandContents(operandStart, state)
        val result = originalValue + 1.toUByte
        writeToOperandLocation(operandStart, state, result)
        state.registers.flags.z = result == 0.toUByte
        state.registers.flags.n = false
        state.registers.flags.h = originalValue.overflowFromBit3(1.toUByte)
      }
    )
  }

  /**
   * DEC_MEM_HL - Decrement the byte pointed to by HL by 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DEC__HL_]]
   */
  case object DEC_MEM_HL extends Instruction(Array(OpCode.DEC_MEM_HL.pattern)) with HasR8Operand {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory(),
      Micro.writeMemory { state =>
        val originalValue = state.memory(state.registers.hl)
        val result = originalValue - 1.toUByte
        state.memory.write(state.registers.hl, result)
        state.registers.flags.z = result == 0.toUByte
        state.registers.flags.n = true
        state.registers.flags.h = originalValue.borrowFromBit4(1.toUByte)
      }
    )
  }

  /**
   * DEC_R8 - Decrement the value in register r8 by 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DEC_r8]]
   */
  case class DEC_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val originalValue = operandContents(operandStart, state)
        val result = originalValue - 1.toUByte
        writeToOperandLocation(operandStart, state, result)
        state.registers.flags.z = result == 0.toUByte
        state.registers.flags.n = true
        state.registers.flags.h = originalValue.borrowFromBit4(1.toUByte)
      }
    )
  }

  /**
   * ADD_A_R8 - Add the value in r8 to A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADD_A,r8]]
   */
  case class ADD_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with AddOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToAdd = operandContents(operandStart, state)
        resolve(state, a, valueToAdd, storeInA = true)
      }
    )
  }

  /**
   * ADC_A_R8 - Add the value in r8 plus the carry flag to A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADC_A,r8]]
   */
  case class ADC_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with AddOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToAdd = operandContents(operandStart, state)
        val carryIn = if (state.registers.flags.c) 1.toUByte else 0.toUByte
        resolve(state, a, valueToAdd, carryIn, storeInA = true)
      }
    )
  }

  /**
   * SUB_A_R8 - Subtract the value in r8 from A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#SUB_A,r8]]
   */
  case class SUB_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with SubOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToSub = operandContents(operandStart, state)
        resolve(state, a, valueToSub, storeInA = true)
      }
    )
  }

  /**
   * SBC_A_R8 - Subtract the value in r8 and the carry flag from A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#SBC_A,r8]]
   */
  case class SBC_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with SubOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToSub = operandContents(operandStart, state)
        val carryIn = if (state.registers.flags.c) 1.toUByte else 0.toUByte
        resolve(state, a, valueToSub, carryIn, storeInA = true)
      }
    )
  }

  /**
   * CP_A_R8 - ComPare the value in A with the value in r8.
   *
   * This subtracts the value in r8 from A and sets flags accordingly, but discards the result.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#CP_A,r8]]
   */
  case class CP_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with SubOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToSub = operandContents(operandStart, state)
        resolve(state, a, valueToSub)
      }
    )
  }

  /**
   * ADD_A_IMM8 - Add the value imm8 (n8) to A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADD_A,n8]]
   */
  case class ADD_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with AddOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToAdd = imm8
        resolve(state, a, valueToAdd, storeInA = true)
      }
    )
  }

  /**
   * ADC_A_IMM8 - Add the value imm8 (n8) plus the carry flag to A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADC_A,n8]]
   */
  case class ADC_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with AddOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToAdd = imm8
        val carryIn = if (state.registers.flags.c) 1.toUByte else 0.toUByte
        resolve(state, a, valueToAdd, carryIn, storeInA = true)
      }
    )
  }

  /**
   * SUB_A_IMM8 - Subtract the value imm8 (n8) from A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#SUB_A,n8]]
   */
  case class SUB_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with SubOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToSub = imm8
        resolve(state, a, valueToSub, storeInA = true)
      }
    )
  }

  /**
   * SBC_A_IMM8 - Subtract the value  imm8 (n8) and the carry flag from A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#SBC_A,n8]]
   */
  case class SBC_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with SubOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToSub = imm8
        val carryIn = if (state.registers.flags.c) 1.toUByte else 0.toUByte
        resolve(state, a, valueToSub, carryIn, storeInA = true)
      }
    )
  }

  /**
   * CP_A_IMM8 - ComPare the value in A with the value imm8 (n8).
   *
   * This subtracts the value imm8 (n8) from A and sets flags accordingly, but discards the result.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#CP_A,n8]]
   */
  case class CP_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with SubOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToSub = imm8
        resolve(state, a, valueToSub)
      }
    )
  }

  /*
   * 16-bit arithmetic instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#16-bit_arithmetic_instructions
   **/

  /**
   * ADD_HL_SP - Add the value in SP to HL.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADD_HL,SP]]
   */
  case object ADD_HL_SP extends Instruction(Array(OpCode.ADD_HL_SP.pattern)) with HasR16Operand {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private var carryFromLow: UByte = 0.toUByte

    override protected[instructions] def micro: Seq[Micro] =
      Seq(
        Micro.fetchOpCode(bytes) { state =>
          val hl = state.registers.hl
          val sp = state.registers.sp
          val lSum = hl.registerLoByte.toInt + sp.registerLoByte.toInt
          carryFromLow = if (lSum > UByte.MaxValue.toInt) 1.toUByte else 0.toUByte
          state.registers.l = lSum.toUByte
          state.registers.flags.h = hl.overflowFromBit11(sp)
        },
        Micro.aluOperation { state =>
          val hSum = state.registers.hl.registerHiByte.toInt +
            state.registers.sp.registerHiByte.toInt +
            carryFromLow.toInt

          state.registers.h = hSum.toUByte
          state.registers.flags.c = hSum > UByte.MaxValue.toInt
          state.registers.flags.n = false
        }
      )
  }

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
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R16 = operand(operandStart)

    private var carryFromLow: UByte = 0.toUByte

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val hl = state.registers.hl
        val op = operandContents(operandStart, state)
        val lSum = hl.registerLoByte.toInt + op.registerLoByte.toInt
        carryFromLow = if (lSum > UByte.MaxValue.toInt) 1.toUByte else 0.toUByte
        state.registers.l = lSum.toUByte
        state.registers.flags.h = hl.overflowFromBit11(op)
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
   * INC_SP - Increment the value in register SP by 1.
   *
   * See INC_R16 for why this takes 2 cycles.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#INC_SP]]
   */
  case object INC_SP extends Instruction(Array(OpCode.INC_SP.pattern)) with HasR16Operand {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state =>
        val original = state.registers.sp
        val result = original + 1.toUShort
        state.registers.sp = result
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
    override val cycles: MCycle = MCycle.Fixed(2)
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
   * DEC_SP - Decrement the value in register SP by 1.
   *
   * See INC_R16 for why this takes 2 cycles.
   */
  case object DEC_SP extends Instruction(Array(OpCode.DEC_SP.pattern)) with HasR16Operand {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state =>
        val original = state.registers.sp
        val result = original - 1.toUShort
        state.registers.sp = result
      }
    )
  }

  /**
   * DEC_R16 - Decrement the value in register r16 by 1.
   *
   * See INC_R16 for why this takes 2 cycles.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DEC_r16]]
   */
  case class DEC_R16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand {
    override val cycles: MCycle = MCycle.Fixed(2)
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        state.registers.a = ~state.registers.a
        state.registers.flags.n = true
        state.registers.flags.h = true
      }
    )
  }

  /**
   * AND_A_MEM_HL - Set A to the bitwise AND between the byte pointed to by HL and A.
   *
   * @see[[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#AND_A,_HL_]]
   */
  case object AND_A_MEM_HL extends Instruction(Array(OpCode.AND_A_MEM_HL.pattern)) with HasR8Operand with AndOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToAnd = state.memory(state.registers.hl)
        val result = a & valueToAnd

        resolve(state, result)
      }
    )
  }

  /**
   * AND_A_R8 - Set A to the bitwise AND between the value in r8 and A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#AND_A,r8]]
   */
  case class AND_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with AndOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToAnd = operandContents(operandStart, state)
        val result = a & valueToAnd

        resolve(state, result)
      }
    )
  }

  /**
   * XOR_A_MEM_HL - Set A to the bitwise XOR between the byte pointed to by HL and A.
   *
   * @see[[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#XOR_A,_HL_]]
   */
  case object XOR_A_MEM_HL extends Instruction(Array(OpCode.XOR_A_MEM_HL.pattern)) with HasR8Operand with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToXor = state.memory(state.registers.hl)
        val result = a ^ valueToXor

        resolve(state, result)
      }
    )
  }

  /**
   * XOR_A_R8 - Set A to the bitwise XOR between the value in r8 and A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#XOR_A,r8]]
   */
  case class XOR_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToXor = operandContents(operandStart, state)
        val result = a ^ valueToXor

        resolve(state, result)
      }
    )
  }

  /**
   * OR_A_MEM_HL - Set A to the bitwise OR between the byte pointed to by HL and A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#OR_A,_HL_]]
   */
  case object OR_A_MEM_HL extends Instruction(Array(OpCode.OR_A_MEM_HL.pattern)) with HasR8Operand with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToOr = state.memory(state.registers.hl)
        val result = a | valueToOr

        resolve(state, result)
      }
    )
  }

  /**
   * OR_A_R8 - Set A to the bitwise OR between the value in r8 and A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#OR_A,r8]]
   */
  case class OR_A_R8(private val input: Array[UByte]) extends Instruction(input) with HasR8Operand with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        val a = state.registers.a
        val valueToOr = operandContents(operandStart, state)
        val result = a | valueToOr

        resolve(state, result)
      }
    )
  }

  /**
   * AND_A_IMM8 - Set A to the bitwise AND between the value imm8 (n8) and A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#AND_A,n8]]
   */
  case class AND_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with AndOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToAnd = imm8
        val result = a & valueToAnd

        resolve(state, result)
      }
    )
  }

  /**
   * XOR_A_IMM8 - Set A to the bitwise XOR between the value imm8 (n8) and A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#XOR_A,n8]]
   */
  case class XOR_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToXor = imm8
        val result = a ^ valueToXor

        resolve(state, result)
      }
    )
  }

  /**
   * OR_A_IMM8 - Set A to the bitwise OR between the value imm8 (n8) and A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#OR_A,n8]]
   */
  case class OR_A_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory { state =>
        val a = state.registers.a
        val valueToOr = imm8
        val result = a | valueToOr

        resolve(state, result)
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
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

  /**
   * JR_IMM8 - Relative Jump to address imm8 (n16 sic).
   *
   * The target address n16 is encoded as a signed 8-bit offset from the address immediately following the JR
   * instruction, so it must be between -128 and 127 bytes away. For example:
   *
   * JR Label  ; no-op; encoded offset of 0
   * Label:
   * JR Label  ; infinite loop; encoded offset of -2
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#JR_n16]]
   */
  case class JR_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemory(),
      Micro.modifyPC { state =>
        val pcAfterInstruction = state.registers.pc
        val offset = imm8.toByte // interpret as signed
        val targetAddress = (pcAfterInstruction.toInt + offset.toInt).toUShort
        state.registers.pc = targetAddress
      }
    )
  }

  /**
   * JR_COND_IMM8 - Relative Jump to address imm8 (n16 sic) if condition cc is met.
   *
   * The target address n16 is encoded as a signed 8-bit offset from the address immediately following the JR
   * instruction, so it must be between -128 and 127 bytes away.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#JR_cc,n16]]
   */
  case class JR_COND_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasCondOperand with HasImm8 {
    override val cycles: MCycle = MCycle.Varying(2 to 3)
    override val bytes: Int = 2

    private val operandStart = 4
    lazy val operand: OpCode.Parameters.Condition = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes),
      Micro.readMemoryAndThen(continueIf(operand)),
      Micro.modifyPC { state =>
        val pcAfterInstruction = state.registers.pc
        val offset = imm8.toByte // interpret as signed
        val targetAddress = (pcAfterInstruction.toInt + offset.toInt).toUShort
        state.registers.pc = targetAddress
      }
    )
  }

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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = true
      }
    )
  }

  case object CCF extends Instruction(Array(OpCode.CCF.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
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
   * LD_MEM_IMM16_SP - Copy SP & $FF at address imm16 (n16) and SP >> 8 at address n16 + 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__n16_,SP]]
   */
  case class LD_MEM_IMM16_SP(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(5)
    override val bytes: Int = 3

    private var originalSP: UShort = UShort.MinValue

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state => originalSP = state.registers.sp },
      Micro.readMemory(),
      Micro.readMemory(),
      Micro.writeMemory { state => state.memory.write(imm16, originalSP.registerLoByte) },
      Micro.writeMemory { state => state.memory.write(imm16 + 1.toUShort, originalSP.registerHiByte) }
    )
  }

  /*
   * Interrupt-related instructions
   * https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#Interrupt-related_instructions
   **/

  /**
   * DI - Disable Interrupts by clearing the IME flag.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DI]]
   */
  case object DI extends Instruction(Array(OpCode.DI.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state => state.setIME(false) },
    )
  }

  /**
   * EI - Enable Interrupts by setting the IME flag.
   *
   * The flag is only set after the instruction following EI.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#EI]]
   */
  case object EI extends Instruction(Array(OpCode.EI.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state => state.setIME(true) },
    )
  }

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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state => ??? } // TODO
    )
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override def executeImplementation(state: Cpu.State): Unit = {}
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
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(bytes) { state =>
        var adjustment = 0
        if (state.registers.flags.n) {
          if (state.registers.flags.h) adjustment += 0x06
          if (state.registers.flags.c) adjustment += 0x60
          adjustment *= -1
        } else {
          if (state.registers.flags.h || (state.registers.a & 0x0F.toUByte) > 0x09.toUByte) adjustment += 0x06
          if (state.registers.flags.c || state.registers.a > 0x99.toUByte) {
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


