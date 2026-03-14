package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import spire.math.UByte
import spire.syntax.literals.*

import scala.util.{Failure, Success, Try}

class InstructionCompletenessTests extends FunSuite {

  private case class BlockRange(start: Int, end: Int, excludes: Set[Int] = Set.empty) {
    val expectedCount: Int = end - start + 1 - excludes.size
  }

  private val blocks = Seq(
    "Block 0" -> BlockRange(0x00, 0x3F),
    "Block 1" -> BlockRange(0x40, 0x7F),
    "Block 2" -> BlockRange(0x80, 0xBF),
    "Block 3" -> BlockRange(0xC0, 0xFF, Set(0xCB)), // 0xCB is a CB prefix, not a standalone opcode
  )

  blocks.foreach { case (name, range) =>
    test(s"$name completed") {

      val validOpcodes = (range.start to range.end).filterNot(range.excludes)
      val results: Seq[(UByte, Try[Instruction])] =
        validOpcodes.map(v => v.toUByte -> Try(Instruction.decode(Array(v.toUByte))))

      val (successes, failures) = results.foldLeft((0, Seq.empty[UByte])) {
        case ((s, fList), (byte, Success(_))) => (s + 1, fList)
        case ((s, fList), (byte, Failure(_))) => (s, fList :+ byte)
      }

      val total = results.size
      val pct = successes * 100 / total
      val failMsg =
        if (failures.nonEmpty)
          s"Failed opcodes: ${failures.map(b => f"0x${b.toInt}%02X").mkString(", ")}"
        else "All opcodes decoded successfully."

      assertEquals(
        successes,
        range.expectedCount,
        clue = s"$name: $successes/$total decoded ($pct%). $failMsg"
      )
    }
  }

  test("CB prefix instructions completed") {
    val results: Seq[(UByte, Try[Instruction])] =
      (0x00 to 0xFF).map(v => v.toUByte -> Try(Instruction.decode(Array(0xCB.toUByte, v.toUByte))))

    val (successes, failures) = results.foldLeft((0, Seq.empty[UByte])) {
      case ((s, fList), (byte, Success(_))) => (s + 1, fList)
      case ((s, fList), (byte, Failure(_))) => (s, fList :+ byte)
    }

    val total = results.size
    val pct = successes * 100 / total
    val failMsg =
      if (failures.nonEmpty)
        s"Failed opcodes: ${failures.map(b => f"0xCB${b.toInt}%02X").mkString(", ")}"
      else "All CB prefix opcodes decoded successfully."

    assertEquals(
      successes,
      256,
      clue = s"CB prefix: $successes/$total decoded ($pct%). $failMsg"
    )
  }

}
