package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.*
import spire.math.UByte

trait BackgroundFetcherFixtures extends PixelFetcherFixtures {

  val TEST_TILE_NUMBER: Int = 5

  def vramIndex(tilemapBase: Int, x: Int, y: Int): Int =
    tilemapBase + y * 32 + x

  def placeTile(state: Ppu.State, tileNumber: Int, tilemapBase: Int = Tile.PRIMARY_TILEMAP_ADDRESS, x: Int = 0, y: Int = 0): Unit =
    state.vram(vramIndex(tilemapBase, x, y)) = UByte(tileNumber)

  def tickBackgroundUntilStepChange(state: Ppu.State, maxTicks: Int = 20): (PixelFetcher.Step, Int) =
    tickUntilStepChange(state, state.backgroundFetcher, maxTicks)

  def runFetcherStepCycle(state: Ppu.State): Seq[UByte] = {
    // GetTileStep (1)-> GetTileDataLowStep (2)-> GetTileDataHighStep (3)-> PushStep (4)-> GetTileStep
    (0 until 4).foreach(_ => tickBackgroundUntilStepChange(state))

    // Now drain the FIFO. Based on previous behavior, it should contain 8 pixels (1 tile).
    val sink = Array.fill(Tile.SIZE)(BackgroundPixel.empty)
    state.backgroundFifo.dequeue(sink)
    sink.toSeq.map(_.colorIndex)
  }
}
