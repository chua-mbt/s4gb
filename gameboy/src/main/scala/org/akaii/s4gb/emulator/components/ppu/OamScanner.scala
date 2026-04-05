package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/**
 * Scans OAM for objects colliding with the scanline
 *
 * @see [[https://gbdev.io/pandocs/OAM.html]]
 */
object OamScanner {

  def scan(state: Ppu.State): Unit = {
    if (state.scanlineDot.current == 0) state.resetScanlineObjects()

    val spriteHeight = LcdControl.spriteHeight(state.registers(Ppu.Address.LCDC))

    if(state.scanlineDot.current % DOTS_PER_SCAN == 0) {
      val oamOffset = state.scanlineDot.current / DOTS_PER_SCAN
      val y = state.oam(oamOffset * OAM_BYTE_SIZE)
      val x = state.oam(oamOffset * OAM_BYTE_SIZE + 1)
      val tileIndex = state.oam(oamOffset * OAM_BYTE_SIZE + 2)
      val attributes = state.oam(oamOffset * OAM_BYTE_SIZE + 3)

      if (inScanline(y, state.ly, spriteHeight)) {
        state.scanlineObjects.find(_.notInUse).foreach(_.set(y, x, tileIndex, attributes))
      }
    }
  }

  private def inScanline(y: UByte, ly: UByte, spriteHeight: Int): Boolean = {
    val objY = y.toInt
    val scanline = ly.toInt

    val top = objY - SPRITE_Y_OFFSET
    val bottom = top + spriteHeight

    scanline >= top && scanline < bottom
  }

  private val DOTS_PER_SCAN: Int = 2
  private val OAM_BYTE_SIZE: Int = 4
  private val SPRITE_Y_OFFSET: Int = 16

  val OBJECTS_PER_SCANLINE: Int = 10
}
