package org.akaii.s4gb.emulator.instructions

import org.akaii.s4gb.emulator.{TestMap, setParam}
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.cpu.Registers
import org.akaii.s4gb.emulator.cpu.Registers.R16
import org.akaii.s4gb.emulator.instructions.{Instruction, OpCode}
import spire.math.{UByte, UShort}

class StackInstructionsTests extends InstructionsTest {

  test("LD_MEM_IMM16_SP") {
    val testMemoryLocations = Seq(0xC000.toUShort, 0xD000.toUShort, 0xE000.toUShort, 0x0000.toUShort, 0xFFFE.toUShort)

    testMemoryLocations.foreach { memoryLocation =>
      val imm16 = memoryLocation
      
      val input: Array[UByte] = Array(OpCode.LD_MEM_IMM16_SP.pattern, imm16.registerLoByte, imm16.registerHiByte)
      val instruction = Instruction.decode(input)

      assertEquals(
        instruction.toString, 
        f"LD_MEM_IMM16_SP(0x${OpCode.LD_MEM_IMM16_SP.pattern.toInt}%02X" +
        f"${imm16.registerLoByte.toInt}%02X${imm16.registerHiByte.toInt}%02X)"
      )
      verifyInstruction[Instruction.LD_MEM_IMM16_SP](OpCode.LD_MEM_IMM16_SP.pattern, instruction) { ld =>
        assertEquals(ld.imm16, imm16)
      }

      val testState = setupTest()
      val testFinal = exhaustInstruction(instruction, testState)

      testInstruction(
        instruction = instruction,
        memoryExpect = memory => {
          memory.write(imm16, UByte(0x00))
          memory.write(imm16 + 1.toUShort, UByte(0x00))
        }
      )
    }
  }

}