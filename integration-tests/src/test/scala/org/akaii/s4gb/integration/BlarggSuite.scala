package org.akaii.s4gb.integration

import org.akaii.s4gb.emulator.Emulator
import org.akaii.s4gb.integration.IntegrationResult.Status

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** blargg's shell prints "Passed" or "Failed" as the last line it manages to write. */
object BlarggSuite extends Suite {

  private val romDir = Paths.get(sys.props("s4gb.roms.blargg"))

  private val resourcePaths = List(
    "cpu_instrs/individual",
    "instr_timing",
    "mem_timing/individual",
    //"mem_timing-2/rom_singles",
  )
  private val resourceFiles = List(
    //"halt_bug.gb",
  )

  private val Passed = "Passed"
  private val Failed = "Failed"

  val name = "blargg"
  val maxCycles = 20000000

  val roms: Either[String, List[Path]] = {
    val missing = resourcePaths.filterNot(dir => Files.isDirectory(romDir.resolve(dir)))
    val listed = for {
      basePath <- resourcePaths
      path <- Files.list(romDir.resolve(basePath)).iterator().asScala
      if path.getFileName.toString.endsWith(".gb")
    } yield s"/$basePath/${path.getFileName}"

    if (missing.nonEmpty) Left(s"ROM directories not found under $romDir: ${missing.mkString(", ")}")
    else Right((listed.toList ++ resourceFiles).sortBy(identity).map(p => romDir.resolve(p.stripPrefix("/"))))
  }

  /** Unlike Mooneye, these ROMs do execute `STOP`, which ends a test as surely as the text does. */
  def completed(machine: Emulator, io: TestMemoryMap): Boolean =
    machine.cpu.isStopped || decided(io.serialOutput)

  def result(rom: Path, machine: Emulator, io: TestMemoryMap, cycles: Int, elapsedNs: Long): IntegrationResult = {
    val serial = io.serialOutput
    val status = statusOf(serial)

    IntegrationResult(name, rom.getFileName.toString, status, cycles, elapsedNs, if (status == Status.Pass) "" else serial)
  }

  private def decided(serial: String): Boolean = serial.contains(Passed) || serial.contains(Failed)

  private def statusOf(serial: String): Status =
    if (serial.contains(Passed)) Status.Pass
    else if (serial.contains(Failed)) Status.Fail
    else Status.Timeout
}

class BlarggTests extends SuiteTests(BlarggSuite)