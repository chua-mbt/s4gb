package org.akaii.s4gb.integration.report

import org.akaii.s4gb.integration.results.IntegrationResult

import java.io.{File, PrintWriter}
import scala.collection.mutable

/**
 * Collects every suite's results in the forked test JVM and renders one page with a
 * section per suite.
 *
 * Suites run in the same JVM, one after another, so there is no single suite that
 * knows when the last one has finished. The page is written from a shutdown hook,
 * which fires as soon as the test JVM exits.
 *
 * Publication is opt in via `-DgenerateReport=true` and the destination comes from
 * `s4gb.reports`, both forwarded into the test JVM by `build.sbt`.
 */
object IntegrationReport {

  private val results = mutable.ListBuffer.empty[IntegrationResult]
  private var hookInstalled = false

  def record(result: IntegrationResult): Unit = synchronized {
    results += result
    if (!hookInstalled) {
      hookInstalled = true
      Runtime.getRuntime.addShutdownHook(new Thread(() => write()))
    }
  }

  private def write(): Unit = {
    if (!publishing) return

    val dir = File(reportDir)
    dir.mkdirs()
    val file = File(dir, reportFile)
    val pw = PrintWriter(file)

    pw.write("<html><head><title>")
    pw.write(escape(title))
    pw.write("</title></head><body>")
    pw.write(s"<h1>${escape(title)}</h1>")

    sections.foreach { (suite, suiteResults) =>
      pw.write(s"<h2>${escape(suite)}</h2>")
      pw.write("<table border=\"1\">")
      pw.write(
        "<tr><th>Test Name</th><th>Cycles</th><th>ns</th><th>ns/cycle</th><th>Status</th><th>Detail</th></tr>"
      )
      suiteResults.foreach { result =>
        pw.write("<tr>")
        val cells = Seq(
          escape(result.name),
          result.cycles.toString,
          result.elapsedNs.toString,
          result.nsPerCycle.toString,
          escape(result.status.toString),
          s"<pre>${escape(result.detail)}</pre>"
        )
        cells.foreach(cell => pw.write(s"<td>$cell</td>"))
        pw.write("</tr>")
      }
      pw.write("</table>")
    }

    pw.write("</body></html>")
    pw.close()
    println(s"Report written to: ${file.getAbsolutePath}")
  }

  /** Suites in the order they first reported, each with its results in report order. */
  private def sections: List[(String, List[IntegrationResult])] = {
    val all = results.toList
    all.map(_.suite).distinct.map(suite => suite -> all.filter(_.suite == suite))
  }

  private def title: String = "s4gb Integration Test Report"

  /**
   * Tests run forked with the subproject directory as their working directory, so
   * a repo-root relative path would land one level too deep. The build passes the
   * absolute location in rather than leaving it to the cwd.
   */
  private val reportDir: String = sys.props.getOrElse("s4gb.reports", "target/reports")

  private val reportFile = "report.html"

  def publishing: Boolean = sys.props.getOrElse("generateReport", "false").toBoolean

  private def escape(text: String): String =
    text.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case c => c.toString
    }.mkString
}