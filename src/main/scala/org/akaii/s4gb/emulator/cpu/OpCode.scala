package org.akaii.s4gb.emulator.cpu

/**
 * Represents an instruction in the Gameboy instruction set.
 *
 * @see [[https://gbdev.io/pandocs/CPU_Instruction_Set.html]]
 */
enum OpCode(val pattern: Int, val mask: Int = 0xFF) {
  // Block 0: https://gbdev.io/pandocs/CPU_Instruction_Set.html#block-0
  case NOP extends OpCode(0x00)                     // 00000000
  case LD_R16_IMM16 extends OpCode(0x01, 0xCF)      // 00DD0001
  case LD_R16MEM_A extends OpCode(0x02, 0xCF)       // 00DD0010
  case LD_A_R16MEM extends OpCode(0x0A, 0xCF)       // 00SS1010
  case LD_MEM_IMM16_SP extends OpCode(0x08)         // 00001000
}

object OpCode {
  def decode(byteValue: Byte): OpCode = {
    val unsigned = byteValue & 0xFF
    values.find(op => (unsigned & op.mask) == op.pattern)
      .getOrElse(throw new RuntimeException(f"Unknown opcode: $unsigned%02X"))
  }
}