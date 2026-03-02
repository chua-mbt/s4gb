package org.akaii.s4gb.emulator.cpu.instructions

import munit.*
import org.akaii.s4gb.emulator.byteops.*
import spire.math.UByte
import spire.syntax.literals.*

import scala.util.{Failure, Success, Try}

class InstructionCompletenessTests extends FunSuite {

  private val blocks = Seq(
    "Block 0" -> (0x00, 0x3F),
    "Block 1" -> (0x40, 0x7F),
    "Block 2" -> (0x80, 0xBF),
    //"Block 3" -> (0xC0, 0xFF) // WIP
  )

  blocks.foreach { case (name, (start, end)) =>
    test(s"$name completed") {

      val results: Seq[(UByte, Try[Instruction])] =
        (start to end).map(v => v.toUByte -> Try(Instruction.decode(Array(v.toUByte))))

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
        64,
        clue = s"$name: $successes/$total decoded ($pct%). $failMsg"
      )
    }
  }

}
