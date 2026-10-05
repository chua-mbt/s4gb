package org.akaii.s4gb.integration.results

import org.akaii.s4gb.integration.results.IntegrationResult.Status

/**
 * Outcome of a single ROM run.
 *
 * Every suite has its own completion protocol, blargg reads "Passed" out of its
 * serial text while Mooneye reads a six byte verdict off the link port. The suite
 * decides which, and hands the answer over here so the reporter can render it
 * without knowing anything about the protocol.
 */
case class IntegrationResult(
  suite: String,
  name: String,
  status: Status,
  cycles: Int,
  elapsedNs: Long,
  detail: String
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