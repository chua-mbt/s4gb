package org.akaii.s4gb.blargg

case class TestResult(
  name: String,
  cycles: Long,
  ns: Long,
  nsPerCycle: Long,
  status: TestResult.TestStatus,
  serialOutput: String = ""
) {
  def summary: String = {
    val output = if (serialOutput.nonEmpty && status != TestResult.TestStatus.Pass) s"\n$serialOutput" else ""
    s"$name: ${status.toString} at $cycles cycles ($ns ns, $nsPerCycle ns/cycle)$output"
  }
}

object TestResult:
  enum TestStatus:
    case Pass, Fail, Timeout
