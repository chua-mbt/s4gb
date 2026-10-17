package org.akaii.s4gb.integration

import org.akaii.s4gb.emulator.components.ppu.{Ppu, PixelEmitter}
import spire.math.UByte

/**
 * Accumulates rendered pixels into a framebuffer and reports frame boundaries.
 *
 * The PPU emits one pixel per visible coordinate per frame and never emits for VBlank
 * lines, so a complete frame is available the moment LY wraps back to 0.
 *
 * @param stableFrames consecutive identical frames that count as settled
 */
class ScreenRecordingEmitter(stableFrames: Int = 10) extends PixelEmitter {

  private val buffer: Array[Array[UByte]] =
    Array.fill(Ppu.VISIBLE_HEIGHT, Ppu.VISIBLE_WIDTH)(UByte(0))

  private var lastLy: Int = -1
  private var stable: Int = 0
  private var completed: List[Frame] = Nil

  override def emit(x: Int, y: Int, color: UByte): Unit =
    if (x >= 0 && x < Ppu.VISIBLE_WIDTH && y >= 0 && y < Ppu.VISIBLE_HEIGHT)
      buffer(y)(x) = color

  /**
   * Folds in a LY sample and, if the scanline just wrapped, banks the frame.
   * Returns the newly completed frame, or None.
   */
  def sampleLy(ly: Int): Option[Frame] = {
    val wrapped = lastLy > 0 && ly == 0
    lastLy = ly
    if (!wrapped) None
    else {
      val pixels = buffer.map(_.clone())
      val differs = completed.headOption.map(previous => countDifferences(previous.pixels, pixels))
      stable = if (differs.contains(0)) stable + 1 else 0
      val frame = Frame(pixels, differs)
      completed = frame :: completed
      Some(frame)
    }
  }

  /** The most recently completed frame. Empty until the first wrap is seen. */
  def lastFrame: Option[Frame] = completed.headOption

  /** Whether the picture has been unchanged for `stableFrames` frames. */
  def settled: Boolean = stable >= stableFrames

  def frameCount: Int = completed.size

  private def countDifferences(a: Array[Array[UByte]], b: Array[Array[UByte]]): Int =
    a.indices
      .map(y => a(y).indices.count(x => a(y)(x) != b(y)(x)))
      .sum
}

/**
 * An immutable snapshot of one frame.
 *
 * @param pixels      shade indices, `pixels(y)(x)`, row major over the visible area
 * @param differsFrom pixel count against the preceding frame, absent for the first
 */
final case class Frame(pixels: Array[Array[UByte]], differsFrom: Option[Int]) {

  val width: Int = Ppu.VISIBLE_WIDTH
  val height: Int = Ppu.VISIBLE_HEIGHT

  def shade(x: Int, y: Int): Int = pixels(y)(x).toInt
}
