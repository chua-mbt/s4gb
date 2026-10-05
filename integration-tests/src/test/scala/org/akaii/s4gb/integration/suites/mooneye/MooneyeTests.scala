package org.akaii.s4gb.integration.suites.mooneye

import munit.FunSuite
import org.akaii.s4gb.emulator.Config
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.components.{Interrupts, Rom, Timer}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, MemoryMap}
import org.akaii.s4gb.integration.report.IntegrationReport
import org.akaii.s4gb.integration.results.IntegrationResult
import org.akaii.s4gb.integration.results.IntegrationResult.Status
import org.akaii.s4gb.integration.{NoopPixelEmitter, TestMemoryMap}
import spire.math.{UByte, UShort}

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec


class MooneyeTests extends FunSuite {

  import MooneyeTests.*

  if (!Files.isDirectory(romRoot)) {
    val skipped = IntegrationResult(
      Suite,
      requestedRoms.mkString(", "),
      Status.Skipped,
      cycles = 0,
      elapsedNs = 0L,
      detail = s"ROMs not found at $romRoot, run: sbt \"integrationTests/mooneyeRoms\""
    )
    IntegrationReport.record(skipped)

    test("mooneye ROMs are available") {
      fail(skipped.detail)
    }
  } else {
    requestedRoms.foreach { path =>
      test(s"run ${path.getFileName}") {
        val result = runMooneyeTest(path)
        val outcome = result.toIntegrationResult
        IntegrationReport.record(outcome)
        println(outcome.summary)

        result.status match {
          case Status.Pass => ()
          case Status.Fail => fail(s"Test reported failure, serial=[${result.hexVerdict}]")
          case Status.Timeout => fail(s"Timed out after ${result.cycles} cycles, serial=[${result.hexVerdict}]")
          case Status.Skipped => ()
        }
      }
    }
  }
}

object MooneyeTests {

  private val ROM_ROOT_PROPERTY = "s4gb.roms.mooneye"
  private val ROM_LIST_PROPERTY = "s4gb.mooneye.roms"


  private val timerTicksPerMCycle = 4
  private val maxCycles = 30000000

  private val AFTER_ROM: UShort = UShort(0x8000)
  private val END_OF_MEMORY: UShort = UShort(0xFFFF)

  /**
   * Mooneye reports pass/fail over the link port as six bytes, and never over
   * any text protocol. A pass sends the Fibonacci numbers to B/C/D/E/H/L, a
   * failure sends `$42` six times. The human readable "Test OK" / "Test failed"
   * lines a test draws go to the LCD, not the serial port.
   *
   * @see https://github.com/Gekkio/mooneye-test-suite README, "Pass/fail reporting"
   */
  private val VerdictLength: Int = 6
  private val Passed: Seq[Byte] = Seq(3, 5, 8, 13, 21, 34)
  private val Failed: Seq[Byte] = Seq.fill(VerdictLength)(0x42.toByte)

  private case class Result(name: String, cycles: Int, elapsedNs: Long, serial: Seq[Byte], ly: UByte) {
    private val verdict: Seq[Byte] = serial.take(VerdictLength)

    def status: Status =
      if (verdict == Passed) {
        Status.Pass
      } else if (verdict == Failed) {
        Status.Fail
      } else {
        Status.Timeout
      }

    def hexVerdict: String = verdict.map(b => f"${b & 0xFF}%02X").mkString(" ")

    /** The verdict bytes are the pass/fail indicator, so they belong in the report. */
    def detail: String = f"verdict=[$hexVerdict], LY=0x${ly.toInt}%02X"

    def toIntegrationResult: IntegrationResult =
      IntegrationResult(Suite, name, status, cycles, elapsedNs, detail)
  }

  private case class Fixtures(cpu: Cpu, ppu: Ppu, timer: Timer, io: TestMemoryMap)

  /** Cache directory as laid down by RomSources.fetch, which nests Mooneye under its build name. */
  val romRoot: Path = {
    val base = Paths.get(sys.props.getOrElse(ROM_ROOT_PROPERTY, s"integration-tests/.rom-cache/mooneye-test-suite"))
    nestedRoot(base)
  }

  private def nestedRoot(base: Path): Path = {
    val roms = Files.list(base)
    val hasRoms =
      try roms.anyMatch(_.toString.endsWith(".gb"))
      finally roms.close()

    if (hasRoms) base
    else {
      val children = Option(base.toFile.listFiles()).getOrElse(Array.empty[File]).filter(_.isDirectory)
      if (children.length == 1) Paths.get(children.head.getPath) else base
    }
  }

  /** Defaults to the one ROM this harness was developed against; override with a comma separated list. */
  private val requestedRoms: List[Path] =
    sys.props.get(ROM_LIST_PROPERTY) match {
      case Some(list) => list.split(",").toList.map(_.trim).filter(_.nonEmpty).map(Paths.get(_))
      case None => List(romRoot.resolve("acceptance/instr/daa.gb"))
    }

  private def runMooneyeTest(rom: Path): Result = {
    val fixtures = createTestFixtures(rom)

    val startTime = System.nanoTime()
    val (cycles, serial) = loop(fixtures, 0)
    val endTime = System.nanoTime()

    Result(rom.getFileName.toString, cycles, endTime - startTime, serial, fixtures.ppu(Ppu.Address.LY))
  }

  /**
   * Drives the machine the way `Emulator.tick` does: one CPU m-cycle, four
   * timer ticks, four PPU dots. Stops once the six verdict bytes are out, which
   * is the point at which the ROM has committed to pass or fail and then spins
   * in an infinite `jr`.
   */
  @tailrec
  private def loop(f: Fixtures, cycles: Int): (Int, Seq[Byte]) = {
    val serial = f.io.serialBytes

    if (cycles >= maxCycles || isDone(f, serial)) (cycles, serial)
    else {
      (0 until timerTicksPerMCycle).foreach { _ =>
        f.timer.tick()
        f.ppu.tick()
      }
      f.cpu.tick()

      loop(f, cycles + 1)
    }
  }

  private def isDone(f: Fixtures, serial: Seq[Byte]): Boolean =
    f.cpu.state.isInstructionBoundary && serial.size >= VerdictLength

  private def createTestFixtures(rom: Path): Fixtures = {
    val romData = Files.readAllBytes(rom).map(b => UByte(b & 0xFF))
    val romComponent = Rom(romData)
    val io = new TestMemoryMap
    val interrupts = Interrupts()
    val timer = Timer(interrupts)
    val ppu = Ppu(interrupts, NoopPixelEmitter)
    val memory = createMemoryDispatcher(romComponent, io, timer, interrupts, ppu)
    val cpu = Cpu(Cpu.State(Registers(), memory, config = Config()))
    cpu.initialize()
    ppu.initialize()
    Fixtures(cpu, ppu, timer, io)
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

  private val Suite: String = "mooneye"
}