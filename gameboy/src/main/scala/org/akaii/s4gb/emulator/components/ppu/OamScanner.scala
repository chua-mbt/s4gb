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

    val objectHeight = state.lcdControl.objectHeight

    val isScanTick = state.scanlineDot.current % DOTS_PER_SCAN == 0
    if (isScanTick) {
      val oamOffset = state.scanlineDot.current / DOTS_PER_SCAN
      val y = state.oam(oamOffset * GameboyObject.BYTE_SIZE)
      val x = state.oam(oamOffset * GameboyObject.BYTE_SIZE + 1)
      val tileIndex = state.oam(oamOffset * GameboyObject.BYTE_SIZE + 2)
      val attributes = state.oam(oamOffset * GameboyObject.BYTE_SIZE + 3)

      if (inScanline(y, state.ly, objectHeight)) {
        state.scanlineObjects.find(_.notInUse).foreach(_.set(y, x, tileIndex, attributes))
      }
    }
  }

  /**
   * Sorts the found objects into fetch order: ascending X, ties by OAM order, since
   * the scan appends into the first free slot so a slot's index is its OAM order.
   *
   * @see [[https://gbdev.io/pandocs/OAM.html#drawing-priority]]
   */
  def sortByDrawingPriority(state: Ppu.State): Unit = {
    val objects = state.scanlineObjects
    val found = objects.indexWhere(_.notInUse) match {
      case -1 => objects.length
      case firstUnused => firstUnused
    }

    insertionSortByX(objects, found)
  }

  /** Ascending X, in place. Stable, so equal X keeps its OAM order, which is why the
   * comparison is `>` and not `>=`. */
  private def insertionSortByX(objects: Array[GameboyObject], count: Int): Unit = {
    var insertAt = 1
    while (insertAt < count) {
      val moving = objects(insertAt)
      val movingX = moving.x.toInt
      var shiftFrom = insertAt - 1
      while (shiftFrom >= 0 && objects(shiftFrom).x.toInt > movingX) {
        objects(shiftFrom + 1) = objects(shiftFrom)
        shiftFrom -= 1
      }
      objects(shiftFrom + 1) = moving
      insertAt += 1
    }
  }

  private def inScanline(y: UByte, ly: UByte, objectHeight: Int): Boolean = {
    val objY = y.toInt
    val scanline = ly.toInt

    val top = GameboyObject.topEdge(objY)
    val bottom = top + objectHeight

    scanline >= top && scanline < bottom
  }

  private[ppu] val DOTS_PER_SCAN: Int = 2

  val OBJECTS_PER_SCANLINE: Int = 10
}
