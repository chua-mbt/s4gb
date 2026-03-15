package org.akaii.s4gb.blargg

import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.UShort

/**
 * Generates Gameboy Doctor compatible logs.
 * @see [[https://github.com/robert/gameboy-doctor]]
 */
trait GameboyDoctor {
  private def hex2(v: Int): String = f"$v%02X"
  private def hex4(v: Int): String = f"$v%04X"

  def formatCpuState(registers: Registers, memory: MemoryMap): String = {
    val a = hex2(registers.a.toInt)
    val f = hex2(registers.f.toInt)
    val b = hex2(registers.b.toInt)
    val c = hex2(registers.c.toInt)
    val d = hex2(registers.d.toInt)
    val e = hex2(registers.e.toInt)
    val h = hex2(registers.h.toInt)
    val l = hex2(registers.l.toInt)
    val sp = hex4(registers.sp.toInt)
    val pc = hex4(registers.pc.toInt)
    
    val pcAddress = registers.pc.toInt
    val pcMemory = (0 until 4)
      .map(i => memory(UShort((pcAddress + i) & 0xFFFF)).toInt)
      .map(hex2)
      .mkString(",")

    s"A:$a F:$f B:$b C:$c D:$d E:$e H:$h L:$l SP:$sp PC:$pc PCMEM:$pcMemory"
  }

  def openLogFile(name: String): java.io.PrintWriter = {
    val dir = java.io.File(GameboyDoctor.logDir)
    dir.mkdirs()
    val file = java.io.File(dir, name)
    java.io.PrintWriter(file)
  }

  def closeLogFile(pw: java.io.PrintWriter): Unit = {
    pw.close()
  }
}

object GameboyDoctor {
  val logDir = "blargg-test/target/logs"
}
