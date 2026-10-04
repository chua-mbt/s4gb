package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/** Discards every emitted pixel. */
object TestEmitters {
  val nullEmitter: PixelEmitter = (_, _, _) => ()
}

/** A discarding [[PixelEmitter]]. */
trait TestEmitters {
  protected val nullEmitter: PixelEmitter = TestEmitters.nullEmitter
}

/** Records every emitted pixel. Emitting outside the frame throws. */
case class RecordingEmitter(
  width: Int = Ppu.VISIBLE_WIDTH,
  height: Int = Ppu.VISIBLE_HEIGHT
) extends PixelEmitter {

  private val frame: Array[Array[UByte]] = Array.fill(height, width)(UByte(0))
  private var emittedCount: Int = 0

  override def emit(x: Int, y: Int, color: UByte): Unit = {
    frame(y)(x) = color
    emittedCount += 1
  }

  def row(y: Int): Seq[UByte] = frame(y).toSeq

  def totalEmitted: Int = emittedCount
}
