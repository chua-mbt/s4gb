package org.akaii.s4gb.integration

import org.akaii.s4gb.emulator.Emulator
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.integration.IntegrationResult.Status

import java.io.File
import java.nio.file.{Files, Path, Paths}

/**
 * Mooneye reports its verdict over the link port as six bytes, never as text. A pass
 * sends the Fibonacci numbers, a failure sends `$42` six times. The "Test OK" lines a
 * test draws go to the LCD, not the serial port.
 *
 * @see https://github.com/Gekkio/mooneye-test-suite README, "Pass/fail reporting"
 */
object MooneyeSuite extends Suite {

  private val ROM_ROOT_PROPERTY = "s4gb.roms.mooneye"
  private val ROM_LIST_PROPERTY = "s4gb.mooneye.roms"

  private val VerdictLength = 6
  private val Passed: Seq[Byte] = Seq(3, 5, 8, 13, 21, 34)
  private val Failed: Seq[Byte] = Seq.fill(VerdictLength)(0x42.toByte)

  val name = "mooneye"
  val maxCycles = 30000000

  /** ROMs known to pass. Everything else is opt in via `-Ds4gb.mooneye.roms`. */
  val DefaultRoms: List[String] = List(
    "acceptance/instr/daa.gb",
    "acceptance/ppu/stat_lyc_onoff.gb",
  )

  private val romRoot: Path = {
    val base = Paths.get(sys.props.getOrElse(ROM_ROOT_PROPERTY, "integration-tests/.rom-cache/mooneye-test-suite"))
    nestedRoot(base)
  }

  val roms: Either[String, List[Path]] =
    if (!Files.isDirectory(romRoot)) Left(s"ROMs not found at $romRoot, run: sbt \"integrationTests/mooneyeRoms\"")
    else Right(requested)

  /** Defaults to the ROMs known to pass; override with a comma separated list. */
  private def requested: List[Path] =
    sys.props.get(ROM_LIST_PROPERTY) match {
      case Some(list) => list.split(",").toList.map(_.trim).filter(_.nonEmpty).map(romRoot.resolve)
      case None => DefaultRoms.map(romRoot.resolve)
    }

  /** Cache directory as laid down by RomSources.fetch, which nests Mooneye under its build name. */
  private def nestedRoot(base: Path): Path = {
    val files = Files.list(base)
    val hasRoms =
      try files.anyMatch(_.toString.endsWith(".gb"))
      finally files.close()

    if (hasRoms) base
    else {
      val children = Option(base.toFile.listFiles()).getOrElse(Array.empty[File]).filter(_.isDirectory)
      if (children.length == 1) Paths.get(children.head.getPath) else base
    }
  }

  /**
   * The ROM commits to a verdict then spins in an infinite `jr`, so six bytes is as
   * final as it gets. `cpu.isStopped` is no use here: Mooneye never executes `STOP`.
   */
  def completed(machine: Emulator, io: TestMemoryMap): Boolean = io.serialBytes.size >= VerdictLength

  def result(rom: Path, machine: Emulator, io: TestMemoryMap, cycles: Int, elapsedNs: Long): IntegrationResult = {
    val all = io.serialBytes
    val failedAt = all.indexOfSlice(Failed)
    // A run that never reports a failure is a pass or a timeout, and the verdict is
    // then whatever the ROM committed to first. Slicing on failedAt would wrap to -1
    // and read backwards off the end.
    val verdict = if (failedAt >= 0) all.slice(failedAt, failedAt + VerdictLength) else all.take(VerdictLength)
    val message = if (failedAt >= 0) all.take(failedAt).map(_.toChar).mkString.trim else ""
    val hexVerdict = verdict.map(b => f"${b & 0xFF}%02X").mkString(" ")
    val ly = machine.ppu(Ppu.Address.LY).toInt

    val status =
      if (verdict == Passed) Status.Pass
      else if (verdict == Failed) Status.Fail
      else Status.Timeout

    val detail = f"verdict=[$hexVerdict], LY=0x$ly%02X" + (if (message.isEmpty) "" else s", msg=$message")
    IntegrationResult(name, rom.getFileName.toString, status, cycles, elapsedNs, detail)
  }
}

class MooneyeTests extends SuiteTests(MooneyeSuite)