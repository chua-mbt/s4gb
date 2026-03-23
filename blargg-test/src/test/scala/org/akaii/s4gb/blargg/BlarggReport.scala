package org.akaii.s4gb.blargg

import java.io.PrintWriter

trait BlarggReport {
  import BlarggReport.*

  def generateReport(results: List[TestResult]): Unit = {
    val dir = java.io.File(reportDir)
    dir.mkdirs()
    val file = java.io.File(dir, reportFile)
    val pw = PrintWriter(file)
    
    pw.write("<html><head><title>Blargg Test Report</title></head><body>")
    pw.write("<table border=\"1\">")
    pw.write("<tr><th>Test Name</th><th>Cycles</th><th>ns</th><th>ns/cycle</th><th>Status</th></tr>")
    
    results.foreach { result =>
      pw.write(s"<tr><td>${result.name}</td><td>${result.runResult.cycles}</td><td>${result.elapsedNs}</td><td>${result.nsPerCycle}</td><td>${result.status.toString}</td></tr>")
    }
    
    pw.write("</table></body></html>")
    pw.close()
    println(s"Report written to: ${file.getAbsolutePath}")
  }
}

object BlarggReport {
  private val reportDir = "blargg-test/target/reports"
  private val reportFile = "report.html"
}
