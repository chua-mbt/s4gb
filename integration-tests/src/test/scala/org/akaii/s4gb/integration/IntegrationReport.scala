package org.akaii.s4gb.integration

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

      // Columns are per section, since a suite either renders visuals or it does not.
      val visualLabels = suiteResults.flatMap(_.visuals.map(_.label)).distinct
      val metricLabels = suiteResults.flatMap(_.metrics.map(_.label)).distinct

      val headers = Seq("Test Name", "Cycles", "ns", "ns/cycle", "Status", "Detail") ++
        visualLabels ++ metricLabels
      pw.write(headers.map(h => s"<th>${escape(h)}</th>").mkString("<tr>", "", "</tr>"))

      suiteResults.foreach { result =>
        val cells = Seq(
          escape(result.name),
          result.cycles.toString,
          result.elapsedNs.toString,
          result.nsPerCycle.toString,
          escape(result.status.toString),
          s"<pre>${escape(result.detail)}</pre>"
        ) ++ visualCells(visualLabels, result) ++ metricCells(metricLabels, result)

        pw.write("<tr>")
        cells.foreach(cell => pw.write(s"<td>$cell</td>"))
        pw.write("</tr>")
      }
      pw.write("</table>")
    }

    pw.write("</body></html>")
    pw.close()
    println(s"Report written to: ${file.getAbsolutePath}")
  }

  /**
   * Thumbnails at 1x, the frame's natural size. Each links to the same file, which the
   * browser then shows at full size, so this needs no script and no lightbox.
   */
  private def visualCells(labels: Seq[String], result: IntegrationResult): Seq[String] =
    labels.map { label =>
      result.visuals.find(_.label == label) match {
        case Some(visual) =>
          val src = escape(visual.file.getFileName.toString)
          s"<figure><a href=\"$src\"><img src=\"$src\" width=\"80\"></a>" +
            s"<figcaption>${escape(label)}</figcaption></figure>"
        case None => ""
      }
    }

  private def metricCells(labels: Seq[String], result: IntegrationResult): Seq[String] =
    labels.map(label => result.metrics.find(_.label == label).map(_.value).getOrElse(""))

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