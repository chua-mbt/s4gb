package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.*
import org.akaii.s4gb.extensions.byteops.toUByte

trait ObjectFetcherFixtures extends PixelFetcherFixtures {

  val TEST_OBJECT_TILE_NUMBER: Int = 5

  val OAM_BG_PRIORITY: Int = 1 << 7
  val OAM_Y_FLIP: Int = 1 << 6
  val OAM_X_FLIP: Int = 1 << 5
  val OAM_PALETTE_1: Int = 1 << 4

  def placeObject(
    state: Ppu.State,
    index: Int = 0,
    y: Int = 16,
    x: Int = 8,
    tileIndex: Int = TEST_OBJECT_TILE_NUMBER,
    attributes: Int = 0
  ): Unit =
    state.scanlineObjects(index).set(y.toUByte, x.toUByte, tileIndex.toUByte, attributes.toUByte)

  def tickObjectUntilStepChange(state: Ppu.State, maxTicks: Int = 20): (PixelFetcher.Step, Int) =
    tickUntilStepChange(state, state.objectFetcher, maxTicks)

  def runObjectFetchStepCycle(state: Ppu.State): Seq[ObjectPixel] = {
    // GetTileStep (1)-> GetTileDataLowStep (2)-> GetTileDataHighStep (3)-> PushStep (4)
    (0 until 4).foreach(_ => tickObjectUntilStepChange(state))

    // Now drain the FIFO. It is padded to 8 pixels (one tile row) on push.
    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    state.objectFifo.dequeue(sink)
    sink.toSeq
  }
}
