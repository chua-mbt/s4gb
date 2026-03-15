package org.akaii.s4gb.blargg

import munit.FunSuite
import org.akaii.s4gb.emulator.cpu.{Cpu, Registers}
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.components.Rom
import org.akaii.s4gb.emulator.components.Rom.Address
import org.akaii.s4gb.emulator.components.io.Joypad
import org.akaii.s4gb.emulator.memorymap.{Dispatcher, MemoryMap}
import spire.math.{UByte, UShort}

class BlarggCpuTests extends FunSuite with GameboyDoctor with BlarggReport {

  import BlarggCpuTests.*

  private val dumpLog = sys.props.getOrElse("gameboyDoctorLogs", "false").toBoolean
  private val generateReport = sys.props.getOrElse("generateReport", "false").toBoolean

  private val testResults = scala.collection.mutable.ListBuffer[TestResult]()

  romFiles.foreach { filename =>
    test(s"run $filename") {
      val bytes = loadRom(filename)
      val (cpu, io) = createCpu(bytes)

      val logFile = Option.when(dumpLog)(openLogFile(filename.replace(".gb", ".log")))

      // Log initial state
      logFile.foreach(_.println(formatCpuState(cpu.state.registers, cpu.state.memory)))

      // Run CPU for a fixed number of cycles
      val startTime = System.nanoTime()
      var cycles = 0
      var result: Option[String] = None
      while (cycles < maxCycles && !cpu.isStopped && result.isEmpty) {
        cpu.tick()
        
        logFile.filter(_ => cpu.state.isInstructionBoundary).foreach(_.println(formatCpuState(cpu.state.registers, cpu.state.memory)))
        
        if (io.serialOutput.contains("Passed") || io.serialOutput.contains("Failed")) {
          result = Some(io.serialOutput)
        }
        
        cycles += 1
      }

      logFile.foreach { fw =>
        closeLogFile(fw)
        println(s"Log written to: ${GameboyDoctor.logDir}/$filename")
      }

      val elapsed = System.nanoTime() - startTime
      val nsPerCycle = elapsed / cycles
      
      val status = result match {
        case Some(r) if r.contains("Passed") => TestResult.TestStatus.Pass
        case Some(r) if r.contains("Failed") => TestResult.TestStatus.Fail
        case _ => TestResult.TestStatus.Timeout
      }
      
      val testResult = TestResult(filename, cycles, elapsed, nsPerCycle, status, io.serialOutput)
      testResults += testResult
      println(testResult.summary)
      
      if (status == TestResult.TestStatus.Fail) fail(result.get)
      if (status == TestResult.TestStatus.Timeout) fail("Timed out")
    }
  }

  override def afterAll(): Unit = {
    if (generateReport) generateReport(testResults.toList)
  }
}

object BlarggCpuTests {
  private val resourcePath = "/cpu_instrs/individual"
  val maxCycles = 20000000

  // TODO: Implement timer (DIV, TIMA, TMA at 0xFF04-0xFF07) and interrupt handling to enable test 02
  // Test 02-interrupts.gb requires the CPU to be able to halt and resume for interrupts
  
  private val romFiles: List[String] = List(
    "01-special.gb",
    // "02-interrupts.gb", // TODO: needs timer + interrupt handling
    "03-op sp,hl.gb",
    "04-op r,imm.gb",
    "05-op rp.gb",
    "06-ld r,r.gb",
    "07-jr,jp,call,ret,rst.gb",
    "08-misc instrs.gb",
    "09-op r,r.gb",
    "10-bit ops.gb",
    "11-op a,(hl).gb"
  )

  private def loadRom(filename: String): Array[Byte] = {
    val is = getClass.getResourceAsStream(s"$resourcePath/$filename")
    assert(is != null, s"Resource $filename not found")
    val bytes = Iterator.continually(is.read()).takeWhile(_ != -1).map(_.toByte).toArray
    is.close()
    bytes
  }

  private def createMemoryDispatcher(romData: Array[Byte], io: BlarggTestMemoryMap): MemoryMap = {
    val rom = Rom(romData.map(UByte(_)))
    val interrupts = Interrupts()
    val joypad = Joypad(interrupts)

    Dispatcher.withRanges(
      (Address.ROM_START, Address.ROM_END) -> rom,
      (UShort(0x8000), UShort(0xFFFF)) -> io,
    )
  }

  private def createCpu(romData: Array[Byte]): (Cpu, BlarggTestMemoryMap) = {
    val io = new BlarggTestMemoryMap()
    val memory = createMemoryDispatcher(romData, io)
    val registers = Registers()
    val state = Cpu.State(registers, memory)
    val cpu = Cpu(state)
    cpu.initialize()
    (cpu, io)
  }
}
