package org.akaii.s4gb.integration

import munit.FunSuite
import org.akaii.s4gb.emulator.{Config, Emulator}
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.components.{Interrupts, Rom, Timer}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, MemoryMap}
import org.akaii.s4gb.integration.IntegrationResult.Status
import spire.math.{UByte, UShort}

import java.nio.file.{Files, Path}

/**
 * A ROM suite: where its ROMs come from, how a ROM says it is done, and how a
 * finished run becomes a reportable result. Everything else is shared.
 */
trait Suite {

  def name: String

  /** m-cycles a single ROM may run before it is reported as a timeout. */
  def maxCycles: Int

  /** The ROMs to run, or why they are unavailable. Left reports `Skipped`, not a failure. */
  def roms: Either[String, List[Path]]

  /** Whether this ROM has committed to a verdict. Only asked on instruction boundaries. */
  def completed(machine: Emulator, io: TestMemoryMap): Boolean

  /** Turns a finished run into the suite-neutral result the reporter renders. */
  def result(rom: Path, machine: Emulator, io: TestMemoryMap, cycles: Int, elapsedNs: Long): IntegrationResult
}

object Suite {

  private val AfterRom: UShort = UShort(0x8000)
  private val EndOfMemory: UShort = UShort(0xFFFF)

  /** Reads a ROM, masking so bytes read as unsigned. */
  def readRom(path: Path): Array[Byte] = Files.readAllBytes(path).map(b => (b & 0xFF).toByte)

  /**
   * Loads a ROM into a ticking machine. The boot ROM is stood in for by starting the
   * CPU at `$0100` with the LCD enabled, so ranges are wired up here by hand.
   */
  def boot(romData: Array[Byte]): (Emulator, TestMemoryMap) = {
    val rom = Rom(romData.map(UByte(_)))
    val io = new TestMemoryMap
    val interrupts = Interrupts()
    val timer = Timer(interrupts)
    val ppu = Ppu(interrupts, NoopPixelEmitter)
    val cpu = Cpu(Cpu.State(Registers(), memory(rom, io, timer, interrupts, ppu), config = Config()))
    cpu.initialize()
    ppu.initialize()
    (Emulator(cpu, ppu, timer), io)
  }

  /**
   * Runs until the suite's completion rule fires or the budget is spent, and returns
   * the m-cycles used. The rule is only asked on an instruction boundary, since
   * mid-instruction the state a suite reads means nothing.
   */
  def run(machine: Emulator, io: TestMemoryMap, maxCycles: Int)(
    completed: (Emulator, TestMemoryMap) => Boolean
  ): Int = {
    @annotation.tailrec
    def loop(cycles: Int): Int = {
      val done = machine.cpu.state.isInstructionBoundary && completed(machine, io)

      if (done || cycles >= maxCycles) cycles
      else {
        machine.tick()
        loop(cycles + 1)
      }
    }

    loop(0)
  }

  private def memory(
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
      (AfterRom, EndOfMemory) -> io,
    )

  }

/** Runs one suite, one munit test per ROM. Suites share a JVM, the report is written on exit. */
abstract class SuiteTests(suite: Suite) extends FunSuite {

  suite.roms match {
    case Left(unavailable) =>
      IntegrationReport.record(
        IntegrationResult(suite.name, unavailable, Status.Skipped, 0, 0L, unavailable)
      )

      test(s"${suite.name} ROMs are available") {
        fail(unavailable)
      }

    case Right(paths) =>
      paths.foreach { rom =>
        test(s"run $rom") {
          val result = execute(suite, rom)
          IntegrationReport.record(result)
          println(result.summary)

          result.status match {
            case Status.Pass | Status.Skipped => ()
            case Status.Fail => fail(result.detail)
            case Status.Timeout => fail(s"Timed out after ${result.cycles} cycles, ${result.detail}")
          }
        }
      }
  }

  private def execute(suite: Suite, rom: Path): IntegrationResult = {
    val (machine, io) = Suite.boot(Suite.readRom(rom))

    val startTime = System.nanoTime()
    val cycles = Suite.run(machine, io, suite.maxCycles)(suite.completed)
    val endTime = System.nanoTime()

    suite.result(rom, machine, io, cycles, endTime - startTime)
  }
}