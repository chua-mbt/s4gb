package org.akaii.s4gb.integration

import org.akaii.s4gb.emulator.Emulator
import org.akaii.s4gb.emulator.components.ppu.{Ppu, PixelEmitter}
import org.akaii.s4gb.integration.IntegrationResult.Status

import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/**
 * dmg-acid2 has no serial verdict and no `STOP`. It draws the face during boot, then sits
 * in a loop, so the only completion rule available is the picture itself: the run is done
 * once the frame has stopped changing. The frame is then compared against the reference
 * image the project ships.
 *
 * @see https://github.com/mattcurrie/dmg-acid2 README
 */
object Acid2Suite extends Suite {

  private val McCYCLES_PER_FRAME = 70224

  val name = "acid2"

  /** Frames a single ROM gets before it counts as a timeout. */
  private val frameBudget = sys.props.get("s4gb.acid2.frames").map(_.toInt).getOrElse(120)

  val maxCycles = frameBudget * McCYCLES_PER_FRAME

  /** Consecutive identical frames that mean the picture has settled. */
  private val stableFrames = sys.props.get("s4gb.acid2.stableFrames").map(_.toInt).getOrElse(10)

  /** Percentage of pixels allowed to differ before the ROM counts as failed. */
  private val tolerancePercent = sys.props.get("s4gb.acid2.tolerance").map(_.toDouble).getOrElse(0.5)

  private val rom = Paths.get(
    sys.props.getOrElse("s4gb.acid2.rom", "integration-tests/.rom-cache/dmg-acid2/dmg-acid2.gb")
  )

  private val reference = Paths.get(
    sys.props.getOrElse(
      "s4gb.acid2.reference",
      "integration-tests/src/test/resources/dmg-acid2/img/reference-dmg.png"
    )
  )

  private val screenshots = Paths.get(sys.props.getOrElse("s4gb.reports", "target/reports"))

  /** One ROM, so one recorder. The suite is a singleton and munit runs its tests in order. */
  private val recorder = new ScreenRecordingEmitter(stableFrames)

  override def emitter: PixelEmitter = recorder

  val roms: Either[String, List[Path]] =
    if (!Files.isRegularFile(rom)) Left(s"ROM not found at $rom, run: sbt \"integrationTests/acid2Roms\"")
    else if (!Files.isRegularFile(reference)) Left(s"Reference image not found at $reference, run: sbt \"integrationTests/romSources\"")
    else Right(List(rom))

  /** Asked on instruction boundaries, so this is where the framebuffer is fed and read. */
  def completed(machine: Emulator, io: TestMemoryMap): Boolean = {
    recorder.sampleLy(machine.ppu(Ppu.Address.LY).toInt)
    recorder.settled
  }

  def result(rom: Path, machine: Emulator, io: TestMemoryMap, cycles: Int, elapsedNs: Long): IntegrationResult =
    recorder.lastFrame match {
      case None =>
        IntegrationResult(name, rom.getFileName.toString, Status.Timeout, cycles, elapsedNs, "no frame completed")
      case Some(frame) =>
        judge(frame, rom.getFileName.toString, cycles, elapsedNs)
    }

  /**
   * Images go beside `report.html` rather than into a screenshots directory of their own,
   * because the workflow publishes the report directory and nothing else. A `<img>` that
   * points outside it renders locally and 404s once deployed.
   */
  private def judge(frame: Frame, romName: String, cycles: Int, elapsedNs: Long): IntegrationResult = {
    Files.createDirectories(screenshots)

    val actual = screenshots.resolve("acid2-actual.png")
    val diff = screenshots.resolve("acid2-diff.png")
    val referenceCopy = screenshots.resolve("acid2-reference.png")

    FrameWriter.write(frame, actual.toFile)
    Files.copy(reference, referenceCopy, StandardCopyOption.REPLACE_EXISTING)

    FrameWriter.readGreyscale(reference.toFile) match {
      case None =>
        IntegrationResult(name, romName, Status.Fail, cycles, elapsedNs, s"reference unreadable at $reference")

      case Some(expected) =>
        val total = frame.width * frame.height
        val count = FrameWriter.writeDiff(frame, expected, diff.toFile)
        val percent = 100.0 * count / total
        val status = if (percent <= tolerancePercent) Status.Pass else Status.Fail

        IntegrationResult(
          name,
          romName,
          status,
          cycles,
          elapsedNs,
          f"$count of $total pixels differ, ${recorder.frameCount} frames rendered",
          visuals = Seq(
            Visual("produced", actual),
            Visual("reference", referenceCopy),
            Visual("diff", diff)
          ),
          metrics = Seq(
            Metric("Pixel diff", f"$percent%.3f%%"),
            Metric("Pixels differing", s"$count of $total"),
            Metric("Tolerance", f"$tolerancePercent%.3f%%")
          )
        )
    }
  }
}

class Acid2Tests extends SuiteTests(Acid2Suite)
