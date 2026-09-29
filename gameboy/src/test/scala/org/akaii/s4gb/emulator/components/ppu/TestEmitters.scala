package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/**
 * Fixture trait to provide a null PixelEmitter for tests.
 */
trait TestEmitters {
  protected val nullEmitter: PixelEmitter = new PixelEmitter {
    override def emit(x: Int, y: Int, color: UByte): Unit = ()
  }
}
