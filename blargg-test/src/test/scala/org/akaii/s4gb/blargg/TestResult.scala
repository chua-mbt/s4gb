package org.akaii.s4gb.blargg

case class TestResult(
  name: String,
  startTimeNs: Long,
  endTimeNs: Long,
  runResult: TestResult.RunResult
) {
  def elapsedNs: Long = endTimeNs - startTimeNs

  def nsPerCycle: Long = elapsedNs / runResult.cycles

  def status: TestResult.TestStatus = runResult.serialOutput match {
    case s if s.contains("Passed") => TestResult.TestStatus.Pass
    case s if s.contains("Failed") => TestResult.TestStatus.Fail
    case _ => TestResult.TestStatus.Timeout
  }

  def summary: String = {
    val output = if (runResult.serialOutput.nonEmpty && status != TestResult.TestStatus.Pass) s"\n${runResult.serialOutput}" else ""
    s"$name: ${status.toString} at ${runResult.cycles} cycles ($elapsedNs ns, $nsPerCycle ns/cycle)$output"
  }
}

object TestResult:
  enum TestStatus:
    case Pass, Fail, Timeout

  case class RunResult(cycles: Int, serialOutput: String)
