package org.akaii.s4gb.emulator.components.ppu

import spire.math.{UByte, UShort}

import scala.collection.mutable

trait PixelFetcherFixtures {

  val TEST_TILE_NUMBER: Int = 5

  def vramIndex(tilemapBase: Int, x: Int, y: Int): Int =
    tilemapBase + y * 32 + x

  def makeState(
    scx: Int = 0,
    scy: Int = 0,
    ly: Int = 0,
    wx: Int = 7,
    wy: Int = 0,
    bgWindowTileData: Boolean = true,
    bgTileMap: Boolean = false,
    windowTileMap: Boolean = false,
    windowEnable: Boolean = false,
    vram: Array[UByte] = Array.fill(Ppu.VRAM_SIZE)(UByte(0))
  ): Ppu.State = {
    val registers = mutable.Map[UShort, UByte](
      Ppu.Address.SCX -> UByte(scx),
      Ppu.Address.SCY -> UByte(scy),
      Ppu.Address.WX -> UByte(wx),
      Ppu.Address.WY -> UByte(wy),
    )
    val lcdc = LcdControl(
      bgWindowTileData = bgWindowTileData,
      bgTileMap = bgTileMap,
      windowTileMap = windowTileMap,
      windowEnable = windowEnable,
    )
    val state = Ppu.State(
      vram = vram,
      registers = registers,
      lcdControl = lcdc,
    )
    state.ly = UByte(ly)
    state
  }

  def placeTile(state: Ppu.State, tileNumber: Int, tilemapBase: Int = Tile.PRIMARY_TILEMAP_ADDRESS, x: Int = 0, y: Int = 0): Unit =
    state.vram(vramIndex(tilemapBase, x, y)) = UByte(tileNumber)

  def tickUntilStepChange(state: Ppu.State, maxTicks: Int = 20): (PixelFetcher.Step, Int) = {
    var ticks = 0
    val initialStep = state.pixelFetcher.step
    while (state.pixelFetcher.step == initialStep && ticks < maxTicks) {
      state.pixelFetcher.step.tick(state)
      ticks += 1
    }
    (state.pixelFetcher.step, ticks)
  }

  def runFetcherStepCycle(state: Ppu.State): Seq[UByte] = {
    // GetTileStep (1)-> GetTileDataLowStep (2)-> GetTileDataHighStep (3)-> SleepStep (4)-> PushStep (5)-> GetTileStep
    (0 until 5).foreach(_ => tickUntilStepChange(state))

    // Now drain the FIFO. Based on previous behavior, it should contain 8 pixels (1 tile).
    val sink = Array.fill(Tile.SIZE)(Pixel(Pixel.BG_PALETTE))
    state.backgroundFifo.dequeue(sink)
    sink.toSeq.map(_.colorIndex)
  }
}
