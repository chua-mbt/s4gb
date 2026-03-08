package org.akaii.s4gb.emulator.cpu.instructions

import org.akaii.s4gb.emulator.MemoryMap
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.instructions.OpCode
import org.akaii.s4gb.emulator.cpu.instructions.OpCode.Extract.*
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
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

  def execute(state: Cpu.State): Instruction.ExecutionResult = {
    val earlyCompletion: Boolean =
      if (state.getMicroStep < micro.length) {
        val microInstruction = micro(state.getMicroStep)
        microInstruction.execute(state) == Instruction.ExecutionResult.Completed
      } else {
        false
      }

    val nextStep = state.getElapsed + 1
    Instruction.ExecutionResult(earlyCompletion || nextStep > micro.length)
  }

  protected[instructions] def micro: Seq[Instruction.Micro] = Seq(Instruction.Micro.fetchOpCode())

  override def toString: String = f"$productPrefix(0x${opCode.toInt}%02X)"
}

object Instruction {
  def decode(input: Array[UByte]): Instruction =
    input.head match {
      case opCode if OpCode.HOLES.contains(opCode) => HOLE(input)
      case OpCode.PREFIXED => decodePrefixed(input.tail)
      case _ => decodeBase(input)
    }

  private def decodeBase(input: Array[UByte]): Instruction =
    OpCode.decode(OpCode.Base.values, input.head) match {
      // Block 0 (0b00)  https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
      case OpCode.Base.NOP => NOP
      case OpCode.Base.LD_SP_IMM16 => LD_SP_IMM16(input)
      case OpCode.Base.LD_R16_IMM16 => LD_R16_IMM16(input)
      case OpCode.Base.LD_R16MEM_A => LD_R16MEM_A(input)
      case OpCode.Base.LD_A_R16MEM => LD_A_R16MEM(input)
      case OpCode.Base.LD_MEM_IMM16_SP => LD_MEM_IMM16_SP(input)
      case OpCode.Base.INC_SP => INC_SP
      case OpCode.Base.INC_R16 => INC_R16(input)
      case OpCode.Base.DEC_SP => DEC_SP
      case OpCode.Base.DEC_R16 => DEC_R16(input)
      case OpCode.Base.ADD_HL_SP => ADD_HL_SP
      case OpCode.Base.ADD_HL_R16 => ADD_HL_R16(input)
      case OpCode.Base.INC_MEM_HL => INC_MEM_HL
      case OpCode.Base.INC_R8 => INC_R8(input)
      case OpCode.Base.DEC_MEM_HL => DEC_MEM_HL
      case OpCode.Base.DEC_R8 => DEC_R8(input)
      case OpCode.Base.LD_MEM_HL_IMM8 => LD_MEM_HL_IMM8(input)
      case OpCode.Base.LD_R8_IMM8 => LD_R8_IMM8(input)
      case OpCode.Base.RLCA => RLCA
      case OpCode.Base.RRCA => RRCA
      case OpCode.Base.RLA => RLA
      case OpCode.Base.RRA => RRA
      case OpCode.Base.DAA => DAA
      case OpCode.Base.CPL => CPL
      case OpCode.Base.SCF => SCF
      case OpCode.Base.CCF => CCF
      case OpCode.Base.JR_IMM8 => JR_IMM8(input)
      case OpCode.Base.JR_COND_IMM8 => JR_COND_IMM8(input)
      case OpCode.Base.STOP => STOP(input)
      // Block 1 (0b01) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-1-8-bit-register-to-register-loads
      case OpCode.Base.HALT => HALT
      case OpCode.Base.LD_MEM_HL_R8 => LD_MEM_HL_R8(input)
      case OpCode.Base.LD_R8_MEM_HL => LD_R8_MEM_HL(input)
      case OpCode.Base.LD_R8_R8 => LD_R8_R8(input)
      // Block 2 (0b10) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-2-8-bit-arithmetic
      case OpCode.Base.ADD_A_R8 => ADD_A_R8(input)
      case OpCode.Base.ADC_A_R8 => ADC_A_R8(input)
      case OpCode.Base.SUB_A_R8 => SUB_A_R8(input)
      case OpCode.Base.SBC_A_R8 => SBC_A_R8(input)
      case OpCode.Base.AND_A_MEM_HL => AND_A_MEM_HL
      case OpCode.Base.AND_A_R8 => AND_A_R8(input)
      case OpCode.Base.XOR_A_MEM_HL => XOR_A_MEM_HL
      case OpCode.Base.XOR_A_R8 => XOR_A_R8(input)
      case OpCode.Base.OR_A_MEM_HL => OR_A_MEM_HL
      case OpCode.Base.OR_A_R8 => OR_A_R8(input)
      case OpCode.Base.CP_A_R8 => CP_A_R8(input)
      // Block 3 (0b11) https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-3
      case OpCode.Base.ADD_A_IMM8 => ADD_A_IMM8(input)
      case OpCode.Base.ADC_A_IMM8 => ADC_A_IMM8(input)
      case OpCode.Base.SUB_A_IMM8 => SUB_A_IMM8(input)
      case OpCode.Base.SBC_A_IMM8 => SBC_A_IMM8(input)
      case OpCode.Base.AND_A_IMM8 => AND_A_IMM8(input)
      case OpCode.Base.XOR_A_IMM8 => XOR_A_IMM8(input)
      case OpCode.Base.OR_A_IMM8 => OR_A_IMM8(input)
      case OpCode.Base.CP_A_IMM8 => CP_A_IMM8(input)
      case OpCode.Base.RET_COND => RET_COND(input)
      case OpCode.Base.RET => RET
      case OpCode.Base.RETI => RETI
      case OpCode.Base.JP_COND_IMM16 => JP_COND_IMM16(input)
      case OpCode.Base.JP_IMM16 => JP_IMM16(input)
      case OpCode.Base.JP_HL => JP_HL
      case OpCode.Base.CALL_COND_IMM16 => CALL_COND_IMM16(input)
      case OpCode.Base.CALL_IMM16 => CALL_IMM16(input)
      case OpCode.Base.RST_TGT3 => RST_TGT3(input)
      case OpCode.Base.POP_R16STK => POP_R16STK(input)
      case OpCode.Base.PUSH_R16STK => PUSH_R16STK(input)
      case OpCode.Base.LDH_MEM_C_A => LDH_MEM_C_A
      case OpCode.Base.LDH_MEM_IMM8_A => LDH_MEM_IMM8_A(input)
      case OpCode.Base.LD_MEM_IMM16_A => LD_MEM_IMM16_A(input)
      case OpCode.Base.LDH_A_MEM_C => LDH_A_MEM_C
      case OpCode.Base.LDH_A_MEM_IMM8 => LDH_A_MEM_IMM8(input)
      case OpCode.Base.LD_A_MEM_IMM16 => LD_A_MEM_IMM16(input)
      case OpCode.Base.ADD_SP_IMM8 => ADD_SP_IMM8(input)
      case OpCode.Base.LD_HL_ADD_SP_IMM8 => LD_HL_ADD_SP_IMM8(input)
      case OpCode.Base.LD_SP_HL => LD_SP_HL
      case OpCode.Base.DI => DI
      case OpCode.Base.EI => EI
    }

  private def decodePrefixed(input: Array[UByte]): Instruction =
    OpCode.decode(OpCode.CB.values, input.head) match {
      case OpCode.CB.RLC_MEM_HL => RLC_MEM_HL(input)
      case OpCode.CB.RLC_R8 => RLC_R8(input)
      case OpCode.CB.RRC_MEM_HL => RRC_MEM_HL(input)
      case OpCode.CB.RRC_R8 => RRC_R8(input)
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

    case object Undefined extends MCycle {
      override def withinCost(elapsed: Int): Boolean = true

      override def maxCost: Int = Int.MaxValue
    }
  }

  private type MicroStep = Cpu.State => Unit
  private type MicroGate = Cpu.State => ExecutionResult

  /** Each MicroStep costs 1 cycle */
  final case class Micro private(execute: MicroGate)

  sealed trait ExecutionResult

  object ExecutionResult {
    case object Completed extends ExecutionResult

    case object Progressing extends ExecutionResult

    def apply(done: Boolean): ExecutionResult = if (done) Completed else Progressing
  }

  /**
   * Types are distinguished only to understand how the cycle is being spent
   *
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595?utm_source=chatgpt.com#fetch-and-stuff]]
   * */
  private object Micro {

    private def advancePC(bytes: Int, state: Cpu.State): Unit = state.registers.advancePC(bytes)

    private def microOp(execute: MicroStep): Micro = Micro { state => execute(state); ExecutionResult.Progressing }

    def fetchOpCode(execute: MicroStep = _ => ()): Micro = microOp { state => advancePC(1, state); execute(state) }

    def fetchImm8(execute: MicroStep = _ => ()): Micro = fetchOpCode(execute)

    def fetchImm8AndThen(execute: MicroGate): Micro = Micro { state => advancePC(1, state); execute(state) }

    def readMemory(execute: MicroStep = _ => ()): Micro = microOp { state => execute(state) }

    def readMemoryAndThen(execute: MicroGate): Micro = Micro { state => execute(state) }

    def writeMemory(execute: MicroStep = _ => ()): Micro = microOp(execute)

    // idu operations can happen in parallel with memory writes, so sometimes this is subsumed within a write
    def iduOperation(execute: MicroStep = _ => ()): Micro = microOp(execute)

    def aluOperation(execute: MicroStep = _ => ()): Micro = microOp(execute)

    def modifyPC(execute: MicroStep = _ => ()): Micro = microOp(execute)
  }

  /**
   * 16-bit extended instructions prefixed by 0xCB. Require 2 fetches rather than 1.
   *
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#cb-prefix]]
   * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html#cb-prefix-instructions]]
   */
  abstract class CBExtension(private val extension: Array[UByte]) extends Instruction(extension) {
    override def toString: String = f"$productPrefix(0xCB${opCode.toInt}%02X)"

    override protected[instructions] def micro: Seq[Instruction.Micro] =
      super.micro ++ Seq(Instruction.Micro.fetchOpCode())
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
        case OpCode.Parameters.R8.MEM_HL => state.memory(state.registers.hl)
        case parameter => state.registers(parameter.toRegister)
      }

    protected def writeToOperandLocation(operandStart: Int, state: Cpu.State, value: UByte): Unit =
      writeToOperandLocation(operand(operandStart), state, value)

    protected def writeToOperandLocation(parameter: OpCode.Parameters.R8, state: Cpu.State, value: UByte): Unit =
      parameter match {
        case OpCode.Parameters.R8.MEM_HL => state.memory.write(state.registers.hl, value)
        case parameter => state.registers.update(parameter.toRegister, value)
      }
  }

  trait HasR16Operand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.R16 = OpCode.Parameters.R16.values(opCode.range(start, start - 1))

    protected def operandContents(operandStart: Int, state: Cpu.State): UShort =
      operandContents(operand(operandStart), state)

    protected def operandContents(operand: OpCode.Parameters.R16, state: Cpu.State): UShort =
      operand match {
        case OpCode.Parameters.R16.SP => state.registers.sp
        case parameter => state.registers(parameter.toRegister)
      }

    protected def writeToOperandLocation(operandStart: Int, state: Cpu.State, value: UShort): Unit =
      writeToOperandLocation(operand(operandStart), state, value)

    protected def writeToOperandLocation(operand: OpCode.Parameters.R16, state: Cpu.State, value: UShort): Unit =
      operand match {
        case OpCode.Parameters.R16.SP => state.registers.sp = value
        case parameter => state.registers.update(parameter.toRegister, value)
      }

    protected def writeToOperandHiLocation(operandStart: Int, state: Cpu.State, value: UShort): Unit =
      writeToOperandHiLocation(operand(operandStart), state, value)

    protected def writeToOperandHiLocation(operand: OpCode.Parameters.R16, state: Cpu.State, value: UShort): Unit =
      operand match {
        case OpCode.Parameters.R16.SP => state.registers.updateSPHi(value.hiByte)
        case parameter => state.registers.update(parameter.toRegister.hi, value.hiByte)
      }

    protected def writeToOperandLoLocation(operandStart: Int, state: Cpu.State, value: UShort): Unit =
      writeToOperandLoLocation(operand(operandStart), state, value)

    protected def writeToOperandLoLocation(operand: OpCode.Parameters.R16, state: Cpu.State, value: UShort): Unit =
      operand match {
        case OpCode.Parameters.R16.SP => state.registers.updateSPLo(value.loByte)
        case parameter => state.registers.update(parameter.toRegister.lo, value.loByte)
      }
  }

  trait HasR16MemOperand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.R16Mem = OpCode.Parameters.R16Mem.values(opCode.range(start, start - 1))

    protected def operandContents(operandStart: Int, state: Cpu.State): UShort =
      operand(operandStart) match {
        case OpCode.Parameters.R16Mem.BC => state.registers.bc
        case OpCode.Parameters.R16Mem.DE => state.registers.de
        case _ => state.registers.hl
      }

    protected def updateIfHL(operandStart: Int, state: Cpu.State): Unit =
      operand(operandStart) match {
        case OpCode.Parameters.R16Mem.HLPlus => state.registers.hl += 1.toUShort
        case OpCode.Parameters.R16Mem.HLMinus => state.registers.hl -= 1.toUShort
        case _ => ()
      }

  }

  trait HasR16StkOperand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.R16Stack = OpCode.Parameters.R16Stack.values(opCode.range(start, start - 1))

    protected def operandContents(operandStart: Int, state: Cpu.State): UShort =
      operandContents(operand(operandStart), state)

    protected def operandContents(operand: OpCode.Parameters.R16Stack, state: Cpu.State): UShort = {
      operand match
        case OpCode.Parameters.R16Stack.BC => state.registers.bc
        case OpCode.Parameters.R16Stack.DE => state.registers.de
        case OpCode.Parameters.R16Stack.HL => state.registers.hl
        case OpCode.Parameters.R16Stack.AF => state.registers.af
    }

    protected def writeToOperandLoLocation(operand: OpCode.Parameters.R16Stack, state: Cpu.State, value: UByte): Unit =
      operand match {
        case OpCode.Parameters.R16Stack.AF => state.registers.f = value
        case r16 => state.registers.update(r16.toRegister.lo, value)
      }

    protected def writeToOperandHiLocation(operand: OpCode.Parameters.R16Stack, state: Cpu.State, value: UByte): Unit =
      operand match {
        case OpCode.Parameters.R16Stack.AF => state.registers.update(Registers.R8.A, value)
        case r16 => state.registers.update(r16.toRegister.hi, value)
      }
  }

  trait HasCondOperand {
    self: Instruction =>
    def operand(start: Int): OpCode.Parameters.Condition = OpCode.Parameters.Condition.values(opCode.range(start, start - 1))

    def continueIf(condition: OpCode.Parameters.Condition)(state: Cpu.State): ExecutionResult = {
      val shouldContinue = condition match {
        case OpCode.Parameters.Condition.NZ => !state.registers.flags.z
        case OpCode.Parameters.Condition.Z => state.registers.flags.z
        case OpCode.Parameters.Condition.NC => !state.registers.flags.c
        case OpCode.Parameters.Condition.C => state.registers.flags.c
      }
      ExecutionResult.apply(!shouldContinue)
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

  trait AddToHLOperation {
    self: Instruction =>

    def addLoBytes(left: UShort, right: UShort, state: Cpu.State): UByte = {
      val lSum = left.loByte.toInt + right.loByte.toInt
      val carry = if (lSum > UByte.MaxValue.toInt) 1.toUByte else 0.toUByte
      state.registers.l = lSum.toUByte
      state.registers.flags.h = left.overflowFromBit11(right)
      carry
    }

    def addHiBytes(left: UShort, right: UShort, carryFromLow: UByte, state: Cpu.State): Unit = {
      val hSum = left.hiByte.toInt + right.hiByte.toInt + carryFromLow.toInt
      state.registers.h = hSum.toUByte
      state.registers.flags.c = hSum > UByte.MaxValue.toInt
      state.registers.flags.n = false
    }
  }

  trait RotateOperation {
    self: Instruction =>

    def setFlags(state: Cpu.State, carry: UByte, resultForZero: Option[UByte] = None): Unit = {
      state.registers.flags.z = resultForZero.contains(0.toUByte)
      state.registers.flags.n = false
      state.registers.flags.h = false
      state.registers.flags.c = carry == 1.toUByte
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
      Micro.fetchImm8 { state => writeToOperandHiLocation(OpCode.Parameters.R16.SP, state, imm16) },
      Micro.fetchImm8 { state => writeToOperandLoLocation(OpCode.Parameters.R16.SP, state, imm16) }
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
      Micro.fetchImm8 { state => writeToOperandLoLocation(destStart, state, imm16) },
      Micro.fetchImm8 { state => writeToOperandHiLocation(destStart, state, imm16) },
    )
  }

  /**
   * LD_R16MEM_A - Copy the value in register A into the byte pointed to by r16.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__r16_,A]]
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__HLI_,A]]
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__HLD_,A]]
   */
  case class LD_R16MEM_A(private val input: Array[UByte]) extends Instruction(input) with HasR16MemOperand {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private val destRefStart = 5
    lazy val destRef: OpCode.Parameters.R16Mem = operand(destRefStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.writeMemory { state =>
        state.memory.write(operandContents(destRefStart, state), state.registers.a)
        updateIfHL(destRefStart, state)
      }
    )
  }

  /**
   * LD_A_R16MEM - Copy the byte pointed to by r16 into register A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_A,_r16_]]
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_A,_HLI_]]
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_A,_HLD_]]
   */
  case class LD_A_R16MEM(private val input: Array[UByte]) extends Instruction(input) with HasR16MemOperand {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private val srcRefStart = 5
    lazy val srcRef: OpCode.Parameters.R16Mem = operand(srcRefStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state =>
        state.registers.a = state.memory(operandContents(srcRefStart, state))
        updateIfHL(srcRefStart, state)
      }
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
      Micro.fetchImm8(),
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
      Micro.fetchImm8 { state => writeToOperandLocation(destStart, state, imm8) }
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
      Micro.fetchOpCode { state => writeToOperandLocation(destStart, state, operandContents(srcStart, state)) }
    )
  }

  /**
   * LDH_MEM_C_A - Copy the value in register A into the byte at address $FF00+C.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LDH__C_,A]]
   */
  case object LDH_MEM_C_A extends Instruction(Array(OpCode.Base.LDH_MEM_C_A.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.writeMemory { state =>
        val address = MemoryMap.IO_REGISTERS_START + state.registers.c.toUShort
        state.memory.write(address, state.registers.a)
      }
    )
  }

  /**
   * LDH_MEM_IMM8_A - Copy the value in register A into the byte at address imm8 (n16).
   *
   * The destination address n16 is encoded as its 8-bit low byte and assumes a high byte of $FF,
   * so it must be between $FF00 and $FFFF.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LDH__n16_,A]]
   */
  case class LDH_MEM_IMM8_A(private val input: Array[UByte]) extends Instruction(input) with HasImm8 {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.writeMemory { state =>
        val address = MemoryMap.IO_REGISTERS_START + imm8.toUShort
        state.memory.write(address, state.registers.a)
      }
    )
  }

  /**
   * LD_MEM_IMM16_A - Copy the value in register A into the byte at address imm16 (n16).
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__n16_,A]]
   */
  case class LD_MEM_IMM16_A(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 3

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.fetchImm8(),
      Micro.writeMemory { state => state.memory.write(imm16, state.registers.a) }
    )
  }

  /**
   * LDH_A_MEM_C - Copy the byte at address $FF00+C into register A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LDH_A,_C_]]
   */
  case object LDH_A_MEM_C extends Instruction(Array(OpCode.Base.LDH_A_MEM_C.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state =>
        val address = MemoryMap.IO_REGISTERS_START + state.registers.c.toUShort
        state.registers.a = state.memory(address)
      }
    )
  }

  /**
   * LDH_A_MEM_IMM8 - Copy the byte at address n16 into register A.
   *
   * The source address n16 is encoded as its 8-bit low byte and assumes a high byte of $FF,
   * so it must be between $FF00 and $FFFF.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LDH_A,_n16_]]
   */
  case class LDH_A_MEM_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.readMemory { state =>
        val address = MemoryMap.IO_REGISTERS_START + imm8.toUShort
        state.registers.a = state.memory(address)
      }
    )
  }

  /**
   * LD_A_MEM_IMM16 - Copy the byte at address imm16 (n16) into register A.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_A,_n16_]]
   */
  case class LD_A_MEM_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 3

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.fetchImm8(),
      Micro.readMemory { state => state.registers.a = state.memory(imm16) }
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
  case object INC_MEM_HL extends Instruction(Array(OpCode.Base.INC_MEM_HL.pattern)) with HasR8Operand {
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
      Micro.fetchOpCode { state =>
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
  case object DEC_MEM_HL extends Instruction(Array(OpCode.Base.DEC_MEM_HL.pattern)) with HasR8Operand {
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
      Micro.fetchOpCode { state =>
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
      Micro.fetchOpCode { state =>
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
      Micro.fetchOpCode { state =>
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
      Micro.fetchOpCode { state =>
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
      Micro.fetchOpCode { state =>
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
      Micro.fetchOpCode { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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
  case object ADD_HL_SP extends Instruction(Array(OpCode.Base.ADD_HL_SP.pattern)) with HasR16Operand with AddToHLOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private var carryFromLow: UByte = 0.toUByte

    override protected[instructions] def micro: Seq[Micro] =
      Seq(
        Micro.fetchOpCode { state =>
          carryFromLow = addLoBytes(state.registers.hl, state.registers.sp, state)
        },
        Micro.aluOperation { state =>
          addHiBytes(state.registers.hl, state.registers.sp, carryFromLow, state)
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
  case class ADD_HL_R16(private val input: Array[UByte]) extends Instruction(input) with HasR16Operand with AddToHLOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R16 = operand(operandStart)

    private var carryFromLow: UByte = 0.toUByte

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val hl = state.registers.hl
        val op = operandContents(operandStart, state)
        carryFromLow = addLoBytes(hl, op, state)
      },
      Micro.aluOperation { state =>
        addHiBytes(state.registers.hl, operandContents(operandStart, state), carryFromLow, state)
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
  case object INC_SP extends Instruction(Array(OpCode.Base.INC_SP.pattern)) with HasR16Operand {
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
  case object DEC_SP extends Instruction(Array(OpCode.Base.DEC_SP.pattern)) with HasR16Operand {
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
  case object CPL extends Instruction(Array(OpCode.Base.CPL.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
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
  case object AND_A_MEM_HL extends Instruction(Array(OpCode.Base.AND_A_MEM_HL.pattern)) with HasR8Operand with AndOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
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
      Micro.fetchOpCode { state =>
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
  case object XOR_A_MEM_HL extends Instruction(Array(OpCode.Base.XOR_A_MEM_HL.pattern)) with HasR8Operand with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
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
      Micro.fetchOpCode { state =>
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
  case object OR_A_MEM_HL extends Instruction(Array(OpCode.Base.OR_A_MEM_HL.pattern)) with HasR8Operand with OrOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
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
      Micro.fetchOpCode { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8 { state =>
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
  case object RLCA extends Instruction(Array(OpCode.Base.RLCA.pattern)) with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carry = (a & 0x80.toUByte) >> 7
        val result = (a << 1) | carry
        state.registers.a = result
        setFlags(state, carry)
      }
    )
  }

  /**
   * RRCA - Rotate register A right.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RRCA]]
   */
  case object RRCA extends Instruction(Array(OpCode.Base.RRCA.pattern)) with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carry = a & 0x01.toUByte
        val result = (a >> 1) | (carry << 7)
        state.registers.a = result
        setFlags(state, carry)
      }
    )
  }

  /**
   * RLA - Rotate register A left, through the carry flag.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RLA]]
   */
  case object RLA extends Instruction(Array(OpCode.Base.RLA.pattern)) with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carryIn = if (state.registers.flags.c) 1.toUByte else 0.toUByte
        val carryOut = (a & 0x80.toUByte) >> 7
        val result = (a << 1) | carryIn
        state.registers.a = result
        setFlags(state, carryOut)
      }
    )
  }

  /**
   * RRA - Rotate register A right, through the carry flag.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RRA]]
   */
  case object RRA extends Instruction(Array(OpCode.Base.RRA.pattern)) with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        val a = state.registers.a
        val carryIn = if (state.registers.flags.c) 0x80.toUByte else 0.toUByte
        val carryOut = a & 0x01.toUByte
        val result = (a >> 1) | carryIn
        state.registers.a = result
        setFlags(state, carryOut)
      }
    )
  }

  /**
   * RLC_MEM_HL - Rotate the byte pointed to by HL left.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RLC__HL_]]
   */
  case class RLC_MEM_HL(private val input: Array[UByte]) extends CBExtension(input) with HasR8Operand with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory(),
      Micro.writeMemory { state =>
        val hl = state.registers.hl
        val value = state.memory(hl)
        val carry = (value & 0x80.toUByte) >> 7
        val result = (value << 1) | carry
        state.memory.write(hl, result)
        setFlags(state, carry, resultForZero = Some(result))
      },
    )
  }

  /**
   * RLC_R8 - Rotate register r8 left.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RLC_r8]]
   */
  case class RLC_R8(private val input: Array[UByte]) extends CBExtension(input) with HasR8Operand with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(),
      Micro.fetchOpCode { state =>
        val value = operandContents(operandStart, state)
        val carry = (value & 0x80.toUByte) >> 7
        val result = (value << 1) | carry
        writeToOperandLocation(operandStart, state, result)
        setFlags(state, carry, resultForZero = Some(result))
      }
    )
  }

  /**
   * RRC_MEM_HL - Rotate the byte pointed to by HL right.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RRC__HL_]]
   */
  case class RRC_MEM_HL(private val input: Array[UByte]) extends CBExtension(input) with HasR8Operand with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory(),
      Micro.writeMemory { state =>
        val hl = state.registers.hl
        val value = state.memory(hl)
        val carry = value & 0x01.toUByte
        val result = (value >> 1) | (carry << 7)
        state.memory.write(hl, result)
        setFlags(state, carry, resultForZero = Some(result))
      }
    )
  }

  /**
   * RRC_R8 - Rotate register r8 right.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RRC_r8]]
   */
  case class RRC_R8(private val input: Array[UByte]) extends CBExtension(input) with HasR8Operand with RotateOperation {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 2

    private val operandStart = 2
    lazy val operand: OpCode.Parameters.R8 = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode(),
      Micro.fetchOpCode { state =>
        val value = operandContents(operandStart, state)
        val carry = value & 0x01.toUByte
        val result = (value >> 1) | (carry << 7)
        writeToOperandLocation(operandStart, state, result)
        setFlags(state, carry, resultForZero = Some(result))
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

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
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
    lazy val condition: OpCode.Parameters.Condition = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8AndThen(continueIf(condition)),
      Micro.modifyPC { state =>
        val pcAfterInstruction = state.registers.pc
        val offset = imm8.toByte // interpret as signed
        val targetAddress = (pcAfterInstruction.toInt + offset.toInt).toUShort
        state.registers.pc = targetAddress
      }
    )
  }

  /**
   * RET_COND - Return from subroutine if condition cc is met.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RET_cc]]
   */
  case class RET_COND(private val input: Array[UByte]) extends Instruction(input) with HasCondOperand {
    override val cycles: MCycle = MCycle.Varying(2 to 5)
    override val bytes: Int = 1

    private val operandStart = 4
    lazy val condition: OpCode.Parameters.Condition = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemoryAndThen(continueIf(condition)),
      Micro.readMemory(),
      Micro.modifyPC { state =>
        state.registers.updatePCLo(state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
      },
      Micro.modifyPC { state =>
        state.registers.updatePCHi(state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
      }
    )
  }

  /**
   * RET - Return from subroutine. This is basically a POP PC (if such an instruction existed).
   * See POP r16 for an explanation of how POP works.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RET]]
   */
  case object RET extends Instruction(Array(OpCode.Base.RET.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory(),
      Micro.modifyPC { state =>
        state.registers.updatePCLo(state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
      },
      Micro.modifyPC { state =>
        state.registers.updatePCHi(state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
      }
    )
  }

  /**
   * RETI - Return from subroutine and enable interrupts. This is basically equivalent to executing EI then RET,
   * meaning that IME is set right after this instruction.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RETI]]
   */
  case object RETI extends Instruction(Array(OpCode.Base.RETI.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory(),
      Micro.modifyPC { state =>
        state.registers.updatePCLo(state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
      },
      Micro.modifyPC { state =>
        state.registers.updatePCHi(state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
        state.setIME(true)
      }
    )
  }

  /**
   * JP_COND_IMM16 - Jump to address imm16 (n16) if condition cc is met.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#JP_cc,n16]]
   */
  case class JP_COND_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasCondOperand with HasImm16 {
    override val cycles: MCycle = MCycle.Varying(3 to 4)
    override val bytes: Int = 3

    private val operandStart = 4
    lazy val condition: OpCode.Parameters.Condition = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.fetchImm8AndThen(continueIf(condition)),
      Micro.modifyPC { state => state.registers.pc = imm16 }
    )
  }

  /**
   * JP_IMM16 - Jump to address imm16 (n16); effectively, copy imm16 (n16) into PC.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#JP_n16]]
   */
  case class JP_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 3

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.fetchImm8(),
      Micro.modifyPC { state => state.registers.pc = imm16 }
    )
  }

  /**
   * JP_HL - Jump to address in HL; effectively, copy the value in register HL into PC.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#JP_HL]]
   */
  case object JP_HL extends Instruction(Array(OpCode.Base.JP_HL.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => state.registers.pc = state.registers.hl },
    )
  }

  /**
   * CALL_COND_IMM16 - Call address n16.
   *
   * This pushes the address of the instruction after the CALL on the stack, such that RET can pop it later;
   * then, it executes an implicit JP n16.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#CALL_cc,n16]]
   */
  case class CALL_COND_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasCondOperand with HasImm16 {
    override val cycles: MCycle = MCycle.Varying(3 to 6)
    override val bytes: Int = 3

    private val operandStart = 4
    lazy val condition: OpCode.Parameters.Condition = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.fetchImm8AndThen(continueIf(condition)),
      Micro.iduOperation { state => state.registers.sp -= 1.toUShort },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, state.registers.pc.hiByte)
        state.registers.sp -= 1.toUShort
      },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, state.registers.pc.loByte)
        state.registers.pc = imm16
      },
    )
  }

  /**
   * CALL_IMM16 - Call address imm16 (n16) if condition cc is met.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#CALL_n16]]
   */
  case class CALL_IMM16(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(6)
    override val bytes: Int = 3

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.fetchImm8(),
      Micro.iduOperation { state => state.registers.sp -= 1.toUShort },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, state.registers.pc.hiByte)
        state.registers.sp -= 1.toUShort
      },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, state.registers.pc.loByte)
        state.registers.pc = imm16
      },
    )
  }

  /**
   * RST_TGT3 - Call address vec. This is a shorter and faster equivalent to CALL for suitable values of vec.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#RST_vec]]
   */
  case class RST_TGT3(private val input: Array[UByte]) extends Instruction(input) {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 1

    private val operandMask = 0b00111000.toUByte
    lazy val targetAddress: UByte = opCode & operandMask

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state => state.registers.sp -= 1.toUShort },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, state.registers.pc.hiByte)
        state.registers.sp -= 1.toUShort
      },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, state.registers.pc.loByte)
        state.registers.pc = targetAddress.toUShort
      },
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
  case object SCF extends Instruction(Array(OpCode.Base.SCF.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        state.registers.flags.n = false
        state.registers.flags.h = false
        state.registers.flags.c = true
      }
    )
  }

  case object CCF extends Instruction(Array(OpCode.Base.CCF.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
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
   * LD_MEM_IMM16_SP - Copy SP & $FF at address imm16 (n16) and SP >> 8 at address n16 + 1.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD__n16_,SP]]
   */
  case class LD_MEM_IMM16_SP(private val input: Array[UByte]) extends Instruction(input) with HasImm16 {
    override val cycles: MCycle = MCycle.Fixed(5)
    override val bytes: Int = 3

    private var originalSP: UShort = UShort.MinValue

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => originalSP = state.registers.sp },
      Micro.fetchImm8(),
      Micro.fetchImm8(),
      Micro.writeMemory { state => state.memory.write(imm16, originalSP.loByte) },
      Micro.writeMemory { state => state.memory.write(imm16 + 1.toUShort, originalSP.hiByte) }
    )
  }

  /**
   * POP_R16STK (POP_AF) - Pop register r16 from the stack. This is roughly equivalent to the following imaginary instructions:
   *
   * LD LOW(r16), [SP]   ; C, E or L
   * INC SP
   * LD HIGH(r16), [SP]  ; B, D or H
   * INC SP
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#POP_r16]]
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#POP_AF]]
   */
  case class POP_R16STK(private val input: Array[UByte]) extends Instruction(input) with HasR16StkOperand {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R16Stack = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.readMemory { state =>
        writeToOperandLoLocation(operand, state, state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
      },
      Micro.readMemory { state =>
        writeToOperandHiLocation(operand, state, state.memory(state.registers.sp))
        state.registers.sp += 1.toUShort
      }
    )
  }

  /**
   * PUSH_R16STK (PUSH_AF) - Push register r16 into the stack. This is roughly equivalent to the following imaginary instructions:
   *
   * DEC SP
   * LD [SP], HIGH(r16)  ; B, D or H
   * DEC SP
   * LD [SP], LOW(r16)   ; C, E or L
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#PUSH_r16]]
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#PUSH_AF]]
   */
  case class PUSH_R16STK(private val input: Array[UByte]) extends Instruction(input) with HasR16StkOperand {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 1

    private val operandStart = 5
    lazy val operand: OpCode.Parameters.R16Stack = operand(operandStart)

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state =>
        state.registers.sp -= 1.toUShort
      },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, operandContents(operand, state).hiByte)
        state.registers.sp -= 1.toUShort
      },
      Micro.writeMemory { state =>
        state.memory.write(state.registers.sp, operandContents(operand, state).loByte)
      }
    )
  }

  /**
   * ADD_SP_IMM8 - Add the signed value e8 to SP.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADD_SP,e8]]
   */
  case class ADD_SP_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 {
    override val cycles: MCycle = MCycle.Fixed(4)
    override val bytes: Int = 2

    private var sum: UShort = 0.toUShort
    private var sp: UShort = 0.toUShort

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
        sp = state.registers.sp
        sum = (sp.toInt + imm8.toByte.toInt).toUShort
      },
      Micro.fetchImm8(),
      Micro.aluOperation { state =>
        state.registers.updateSPLo(sum.loByte)
        state.registers.flags.z = false
        state.registers.flags.n = false
        // The flags are set from unsigned addition, even though imm8 is signed
        state.registers.flags.h = sp.loByte.overflowFromBit3(imm8)
        state.registers.flags.c = sp.loByte.overflowFromBit7(imm8)
      },
      Micro.aluOperation { state =>
        state.registers.updateSPHi(sum.hiByte)
      },
    )
  }

  /**
   * LD_HL_ADD_SP_IMM8 - Add the signed value e8 to SP and copy the result in HL.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_HL,SP+e8]]
   */
  case class LD_HL_ADD_SP_IMM8(private val input: Array[UByte]) extends Instruction(input) with HasImm8 {
    override val cycles: MCycle = MCycle.Fixed(3)
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.fetchImm8(),
      Micro.aluOperation { state =>
        val sp = state.registers.sp
        state.registers.hl = (sp.toInt + imm8.toByte.toInt).toUShort
        state.registers.flags.z = false
        state.registers.flags.n = false
        // The flags are set from unsigned addition, even though imm8 is signed
        state.registers.flags.h = sp.loByte.overflowFromBit3(imm8)
        state.registers.flags.c = sp.loByte.overflowFromBit7(imm8)
      },
    )
  }

  /**
   * LD_SP_HL - Copy register HL into register SP.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#LD_SP,HL]]
   */
  case object LD_SP_HL extends Instruction(Array(OpCode.Base.LD_SP_HL.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state => state.registers.sp = state.registers.hl },
    )
  }

  /**
   * ADD_HL_SP - Add the value in SP to HL.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#ADD_HL,SP]]
   */
  case class ADD_HL_SP(private val input: Array[UByte]) extends Instruction(input) {
    override val cycles: MCycle = MCycle.Fixed(2)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = super.micro ++ Seq(
      Micro.iduOperation { state =>
        val hl = state.registers.hl
        val sp = state.registers.sp
        val result = hl + sp
        state.registers.hl = result
        state.registers.flags.n = false
        state.registers.flags.h = (hl & 0x0FFF.toUShort) + (sp & 0x0FFF.toUShort) > 0x0FFF.toUShort
        state.registers.flags.c = result < hl // overflow means result wrapped around, making it smaller than hl
      },
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
  case object DI extends Instruction(Array(OpCode.Base.DI.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => state.setIME(false) },
    )
  }

  /**
   * EI - Enable Interrupts by setting the IME flag.
   *
   * The flag is only set after the instruction following EI.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#EI]]
   */
  case object EI extends Instruction(Array(OpCode.Base.EI.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => state.setIME(true) },
    )
  }

  /**
   * HALT - Enter CPU low-power consumption mode until an interrupt occurs.
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
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#halt]]
   */
  case object HALT extends Instruction(Array(OpCode.Base.HALT.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => state.changeExecutionMode(Cpu.ExecutionMode.Halted) }
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
  }

  /**
   * DAA - Decimal Adjust Accumulator.
   *
   * Designed to be used after performing an arithmetic instruction (ADD, ADC, SUB, SBC) whose inputs were in
   * Binary-Coded Decimal (BCD), adjusting the result to likewise be in BCD.
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#DAA]]
   */
  case object DAA extends Instruction(Array(OpCode.Base.DAA.pattern)) {
    override val cycles: MCycle = MCycle.Fixed(1)
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state =>
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

  /**
   * STOP - Enter CPU very low power mode. Also used to switch between GBC double speed and normal speed CPU modes.
   *
   * The exact behavior of this instruction is fragile and may interpret its second byte as a separate
   * instruction (see the Pan Docs), which is why rgbasm(1) allows explicitly specifying the
   * second byte (STOP imm8 (n8)) to override the default of $00 (a NOP instruction).
   *
   * @see [[https://rgbds.gbdev.io/docs/v1.0.1/gbz80.7#STOP]]
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#nop-and-stop]]
   */
  case class STOP(private val input: Array[UByte]) extends Instruction(input) with HasImm8 {
    override val cycles: MCycle = MCycle.Undefined
    override val bytes: Int = 2

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => state.changeExecutionMode(Cpu.ExecutionMode.Stopped) },
      Micro.fetchImm8(),
    )
  }

  /*
   * Pseudo-instructions
   **/

  /**
   * OpCode holes (not implemented opcodes)
   *
   * Why non-implemented opcodes hang is simple: they never fetch (in fact, their decode ROM yields all zeroes, which
   * makes it hang in all zeroes due to state s000 having no fetch, or any jump to fetch).
   *
   * Because they never fetch, they effectively wedge the CPU, including interrupts, and even NMI, as ISR and NMI
   * servicing is done at fetch-time!
   *
   * Implemented in this emulator as an instruction that sets CPU state to hard-lock.
   *
   * @see [[https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595#opcode-holes-not-implemented-opcodes]]
   */
  case class HOLE(private val input: Array[UByte]) extends Instruction(input) {
    override val cycles: MCycle = MCycle.Undefined
    override val bytes: Int = 1

    override protected[instructions] def micro: Seq[Micro] = Seq(
      Micro.fetchOpCode { state => state.changeExecutionMode(Cpu.ExecutionMode.HardLock) }
    )
  }

}



