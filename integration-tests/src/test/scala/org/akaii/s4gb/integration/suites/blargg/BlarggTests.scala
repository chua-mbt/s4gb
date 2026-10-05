package org.akaii.s4gb.integration.suites.blargg

import munit.FunSuite
import org.akaii.s4gb.emulator.Config
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.components.{Interrupts, Rom, Timer}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, MemoryMap}
import org.akaii.s4gb.integration.{NoopPixelEmitter, TestMemoryMap}
import org.akaii.s4gb.integration.report.IntegrationReport
import org.akaii.s4gb.integration.results.IntegrationResult
import org.akaii.s4gb.integration.results.IntegrationResult.Status
import spire.math.{UByte, UShort}

import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

class BlarggTests extends FunSuite {

  import BlarggTests.*

  romFiles.foreach { path =>
    test(s"run $path") {
      val bytes = loadRom(path)
      val fixtures = createTestFixtures(bytes)

      val startTime = System.nanoTime()
      val runResult = runBlarggTest(fixtures)
      val endTime = System.nanoTime()

      val status = statusOf(runResult)
      val detail = if (status == Status.Pass) "" else runResult.serialOutput
      val testResult = IntegrationResult(Suite, path, status, runResult.cycles, endTime - startTime, detail)
      IntegrationReport.record(testResult)
      println(testResult.summary)

      if (status == Status.Fail) fail(runResult.serialOutput)
      if (status == Status.Timeout) fail("Timed out")
    }
  }
}

object BlarggTests {
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
  val maxCycles = 20000000

  private val timerTicksPerMCycle = 4

  private val AFTER_ROM: UShort = UShort(0x8000)
  private val END_OF_MEMORY: UShort = UShort(0xFFFF)

  private case class TestFixtures(cpu: Cpu, ppu: Ppu, io: TestMemoryMap, timer: Timer)

  private case class RunResult(cycles: Int, serialOutput: String)

  /** blargg's shell prints "Passed" or "Failed" as the last line it manages to write. */
  private def statusOf(run: RunResult): Status = run.serialOutput match {
    case s if s.contains("Passed") => Status.Pass
    case s if s.contains("Failed") => Status.Fail
    case _ => Status.Timeout
  }

  @tailrec
  private def runBlarggTest(
    fixtures: TestFixtures,
    cycles: Int = 0
  ): RunResult = {
    val serialOutput = fixtures.io.serialOutput
    val completed = fixtures.cpu.state.isInstructionBoundary && (serialOutput.contains("Passed") || serialOutput.contains("Failed"))

    if (cycles >= maxCycles || completed || fixtures.cpu.isStopped) {
      return RunResult(cycles, serialOutput)
    }

    (0 until timerTicksPerMCycle).foreach { _ =>
      fixtures.timer.tick()
      fixtures.ppu.tick()
    }
    fixtures.cpu.tick()

    runBlarggTest(fixtures, cycles + 1)
  }

  private def romFiles: List[String] = {
    val dirFiles = for {
      basePath <- resourcePaths
      path <- Files.list(romDir.resolve(basePath)).iterator().asScala
      if path.getFileName.toString.endsWith(".gb")
    } yield s"/$basePath/${path.getFileName}"
    (dirFiles ++ resourceFiles).sortBy(identity)
  }

  private def loadRom(path: String): Array[Byte] = {
    val rom: Path = romDir.resolve(path.stripPrefix("/"))
    Files.readAllBytes(rom).map(b => (b & 0xFF).toByte)
  }

  private def createMemoryDispatcher(
    rom: Rom,
    io: TestMemoryMap,
    timer: Timer,
    interrupts: Interrupts,
    ppu: Ppu
  ): MemoryMap =
    Dispatcher.withRanges(
      (Rom.Address.ROM_START, Rom.Address.ROM_END) -> rom,
      (Ppu.Address.VRAM.START, Ppu.Address.VRAM.END) -> ppu,
      (Ppu.Address.OAM.START, Ppu.Address.OAM.END) -> ppu,
      (Ppu.Address.LCDC, Ppu.Address.LYC) -> ppu,
      (Ppu.Address.BGP, Ppu.Address.WX) -> ppu,
      (Timer.Address.TIMER_START, Timer.Address.TIMER_END) -> timer,
      (Interrupts.Address.INTERRUPT_FLAG, Interrupts.Address.INTERRUPT_FLAG) -> interrupts,
      (Interrupts.Address.INTERRUPT_ENABLE, Interrupts.Address.INTERRUPT_ENABLE) -> interrupts,
      (AFTER_ROM, END_OF_MEMORY) -> io,
    )

  private def createTestFixtures(romData: Array[Byte]): TestFixtures = {
    val rom = Rom(romData.map(UByte(_)))
    val io = new TestMemoryMap
    val interrupts = Interrupts()
    val timer = Timer(interrupts)
    val ppu = Ppu(interrupts, NoopPixelEmitter)
    val memory = createMemoryDispatcher(rom, io, timer, interrupts, ppu)
    val registers = Registers()
    val state = Cpu.State(registers, memory, config = Config())
    val cpu = Cpu(state)
    cpu.initialize()
    ppu.initialize()
    TestFixtures(cpu, ppu, io, timer)
  }

  private val Suite: String = "blargg"
}