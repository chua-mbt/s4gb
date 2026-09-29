package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/**
 * Interface for emitting pixels to the display.
 */
trait PixelEmitter {
  def emit(x: Int, y: Int, color: UByte): Unit
}
