package org.akaii.s4gb.blargg

import munit.FunSuite
import org.akaii.s4gb.emulator.components.{Interrupts, Rom, Timer}
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, MemoryMap}
import spire.math.{UByte, UShort}

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class BlarggCpuTests extends FunSuite with BlarggReport {

  import BlarggCpuTests.*

  private val generateReport = sys.props.getOrElse("generateReport", "false").toBoolean

  private val testResults = mutable.ListBuffer.empty[TestResult]

  romFiles.foreach { filename =>
    test(s"run $filename") {
      val bytes = loadRom(filename)
      val fixtures = createTestFixtures(bytes)

      val startTime = System.nanoTime()
      val runResult = runBlarggTest(fixtures)
      val endTime = System.nanoTime()

      val testResult = TestResult(filename, startTime, endTime, runResult)
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

object BlarggCpuTests {
  private val resourcePath = "/cpu_instrs/individual"
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

    (0 to timerTicksPerMCycle).foreach(_ => fixtures.timer.tick())
    fixtures.cpu.tick()

    runBlarggTest(fixtures, cycles + 1)
  }

  private def romFiles: List[String] =
    Files.list(Paths.get(getClass.getResource(resourcePath).toURI))
      .filter(_.getFileName.toString.endsWith(".gb"))
      .map(_.getFileName.toString)
      .sorted
      .iterator
      .asScala
      .toList

  private def loadRom(filename: String): Array[Byte] = {
    val inputStream = getClass.getResourceAsStream(s"$resourcePath/$filename")
    assert(inputStream != null, s"Resource $filename not found")
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
    val state = Cpu.State(registers, memory)
    val cpu = Cpu(state)
    cpu.initialize()
    TestFixtures(cpu, io, timer)
  }
}
