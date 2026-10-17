package org.akaii.s4gb.integration

import org.akaii.s4gb.integration.IntegrationResult.Status

import java.nio.file.Path

/**
 * An image a suite produced, shown as a thumbnail beside its row. The file has to sit
 * beside `report.html`, since the workflow publishes the report directory wholesale and
 * the reporter refers to images by bare filename.
 */
case class Visual(label: String, file: Path)

/** A labelled figure for a result row, e.g. a pixel diff percentage. */
case class Metric(label: String, value: String)

/**
 * Outcome of a single ROM run.
 *
 * Every suite has its own completion protocol, blargg reads "Passed" out of its
 * serial text while Mooneye reads a six byte verdict off the link port. The suite
 * decides which, and hands the answer over here so the reporter can render it
 * without knowing anything about the protocol.
 *
 * @param visuals images for suites judged on their rendered output, empty otherwise
 * @param metrics labelled figures for the same reason
 */
case class IntegrationResult(
  suite: String,
  name: String,
  status: Status,
  cycles: Int,
  elapsedNs: Long,
  detail: String,
  visuals: Seq[Visual] = Seq.empty,
  metrics: Seq[Metric] = Seq.empty
) {

  def nsPerCycle: Long = if (cycles <= 0) 0L else elapsedNs / cycles

  def summary: String = {
    val timing = f"$name: $status at $cycles cycles ($elapsedNs ns, $nsPerCycle ns/cycle)"
    if (detail.isEmpty) {
      timing
    } else if (detail.contains('\n')) {
      s"$timing\n$detail"
    } else {
      s"$timing, $detail"
    }
  }
}

object IntegrationResult {

  enum Status {
    /** The ROM reported success through its own protocol. */
    case Pass

    /** The ROM reported failure through its own protocol. */
    case Fail

    /** The ROM never got as far as reporting anything. */
    case Timeout

    /** The ROMs for this suite were not present, so nothing was run. */
    case Skipped
  }
}