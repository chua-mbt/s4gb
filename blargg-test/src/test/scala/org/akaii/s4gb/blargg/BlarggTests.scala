package org.akaii.s4gb.blargg

import munit.FunSuite
import org.akaii.s4gb.emulator.Config
import org.akaii.s4gb.emulator.components.{Interrupts, Rom, Timer}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, MemoryMap}
import spire.math.{UByte, UShort}

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class BlarggTests extends FunSuite with BlarggReport {

  import BlarggTests.*

  private val generateReport = sys.props.getOrElse("generateReport", "false").toBoolean

  private val testResults = mutable.ListBuffer.empty[TestResult]

  romFiles.foreach { path =>
    test(s"run $path") {
      val bytes = loadRom(path)
      val fixtures = createTestFixtures(bytes)

      val startTime = System.nanoTime()
      val runResult = runBlarggTest(fixtures)
      val endTime = System.nanoTime()

      val testResult = TestResult(path, startTime, endTime, runResult)
      testResults += testResult
      println(testResult.summary)

      if (testResult.status == TestResult.TestStatus.Fail) fail(runResult.serialOutput)
      if (testResult.status == TestResult.TestStatus.Timeout) fail("Timed out")
    }
  }

  override def afterAll(): Unit = {
    if (generateReport) generateReport(testResults.toList)
  }
}

object BlarggTests {
  private val resourcePaths = List(
    "/cpu_instrs/individual",
    "/instr_timing/",
    "/mem_timing/individual",
    //"/mem_timing-2/rom_singles",
  )
  private val resourceFiles = List(
    //"/halt_bug.gb",
  )
  val maxCycles = 20000000

  private val timerTicksPerMCycle = 4

  private val AFTER_ROM = UShort(0x8000)
  private val END_OF_MEMORY = UShort(0xFFFF)

  private case class TestFixtures(cpu: Cpu, io: BlarggTestMemoryMap, timer: Timer)

  @tailrec
  private def runBlarggTest(
    fixtures: TestFixtures,
    cycles: Int = 0
  ): TestResult.RunResult = {
    val serialOutput = fixtures.io.serialOutput
    val completed = fixtures.cpu.state.isInstructionBoundary && (serialOutput.contains("Passed") || serialOutput.contains("Failed"))

    if (cycles >= maxCycles || completed || fixtures.cpu.isStopped) {
      return TestResult.RunResult(cycles, serialOutput)
    }

    (0 until timerTicksPerMCycle).foreach(_ => fixtures.timer.tick())
    fixtures.cpu.tick()

    runBlarggTest(fixtures, cycles + 1)
  }

  private def romFiles: List[String] = {
    val dirFiles = for {
      basePath <- resourcePaths
      path <- Files.list(Paths.get(getClass.getResource(basePath).toURI)).iterator().asScala
      if path.getFileName.toString.endsWith(".gb")
    } yield s"$basePath/${path.getFileName}"
    (dirFiles ++ resourceFiles).sortBy(identity)
  }

  private def loadRom(path: String): Array[Byte] = {
    val inputStream = getClass.getResourceAsStream(path)
    val bytes = Iterator.continually(inputStream.read()).takeWhile(_ != -1).map(_.toByte).toArray
    inputStream.close()
    bytes
  }

  private def createMemoryDispatcher(
    rom: Rom,
    io: BlarggTestMemoryMap,
    timer: Timer,
    interrupts: Interrupts
  ): MemoryMap =
    Dispatcher.withRanges(
      (Rom.Address.ROM_START, Rom.Address.ROM_END) -> rom,
      (Timer.Address.TIMER_START, Timer.Address.TIMER_END) -> timer,
      (Interrupts.Address.INTERRUPT_FLAG, Interrupts.Address.INTERRUPT_FLAG) -> interrupts,
      (Interrupts.Address.INTERRUPT_ENABLE, Interrupts.Address.INTERRUPT_ENABLE) -> interrupts,
      (AFTER_ROM, END_OF_MEMORY) -> io,
    )

  private def createTestFixtures(romData: Array[Byte]): TestFixtures = {
    val rom = Rom(romData.map(UByte(_)))
    val io = new BlarggTestMemoryMap()
    val interrupts = Interrupts()
    val timer = Timer(interrupts)
    val memory = createMemoryDispatcher(rom, io, timer, interrupts)
    val registers = Registers()
    val state = Cpu.State(registers, memory, config = Config())
    val cpu = Cpu(state)
    cpu.initialize()
    TestFixtures(cpu, io, timer)
  }
}
