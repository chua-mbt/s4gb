package org.akaii.s4gb.integration

import org.akaii.s4gb.emulator.components.ppu.PixelEmitter
import spire.math.UByte

/** Discards rendered pixels. Suites assert on state, not on output pixels. */
object NoopPixelEmitter extends PixelEmitter {
  override def emit(x: Int, y: Int, color: UByte): Unit = ()
}