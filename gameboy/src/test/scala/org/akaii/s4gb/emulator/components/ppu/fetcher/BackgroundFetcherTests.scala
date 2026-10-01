package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.*
import munit.FunSuite
import org.akaii.s4gb.extensions.byteops.toUByte
import spire.math.UByte

class BackgroundFetcherTests extends FunSuite {

  import BackgroundFetcherTests.*

  test("GetTileStep reads tile number and tileDataAddress, transitions to GetTileDataLowStep") {
    val state = makeState(ly = 3)
    placeTile(state, tileNumber = TEST_TILE_NUMBER)
    state.backgroundFetcher.step = BackgroundFetcher.GetTileStep

    val (next, dots) = tickBackgroundUntilStepChange(state)
    assertEquals(next, BackgroundFetcher.GetTileDataLowStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.backgroundFetcher.tile.tileNumber, TEST_TILE_NUMBER)
    // 8 * 2 * 5 + 3 * 2 = 86
    assertEquals(state.backgroundFetcher.tile.tileDataAddress, 86)
  }

  test("GetTileDataLowStep reads low byte, transitions to GetTileDataHighStep") {
    val state = makeState(ly = 0)
    val expectedTileDataLow = UByte(0xAB)
    state.backgroundFetcher.tile.tileDataAddress = 100
    state.vram(100) = expectedTileDataLow
    state.backgroundFetcher.step = BackgroundFetcher.GetTileDataLowStep

    val (next, dots) = tickBackgroundUntilStepChange(state)
    assertEquals(next, BackgroundFetcher.GetTileDataHighStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.backgroundFetcher.tile.tileDataLow, expectedTileDataLow)
  }

  test("GetTileDataHighStep reads high byte, transitions to PushStep") {
    val state = makeState(ly = 0)
    val expectedTileDataHigh = UByte(0xCD)
    state.backgroundFetcher.tile.tileDataAddress = 100
    state.vram(101) = expectedTileDataHigh
    state.backgroundFetcher.step = BackgroundFetcher.GetTileDataHighStep

    val (next, dots) = tickBackgroundUntilStepChange(state)
    assertEquals(next, BackgroundFetcher.PushStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.backgroundFetcher.tile.tileDataHigh, expectedTileDataHigh)
  }

  test("PushStep extracts 8 pixels MSB first, advances fetcherX, transitions to GetTileStep") {
    val state = makeState(ly = 0)
    state.backgroundFetcher.tile.tileDataLow = UByte(0x8F)
    state.backgroundFetcher.tile.tileDataHigh = UByte(0xF1)
    state.backgroundFetcher.fetcherX = 0
    state.backgroundFetcher.step = BackgroundFetcher.PushStep

    state.backgroundFetcher.step.tick(state)
    assertEquals(state.backgroundFetcher.step, BackgroundFetcher.GetTileStep) // advanced
    assertEquals(state.backgroundFetcher.fetcherX, 1)
    assertEquals(state.backgroundFifo.size, Tile.SIZE)
    // ring buffer pixels and fetcher pixels must be distinct references
    val ringBufferSink = Array.fill(Tile.SIZE)(BackgroundPixel.empty)
    assertEquals(state.backgroundFifo.dequeue(ringBufferSink), Tile.SIZE)
    assertEquals(state.backgroundFifo.size, 0)
    val ringBufferPixels = ringBufferSink.toSeq
    val fetcherPixels = state.backgroundFetcher.pixels.toSeq
    val allIdentities = (ringBufferPixels ++ fetcherPixels).map(System.identityHashCode(_))
    assertEquals(allIdentities.toSet.size, Tile.SIZE * 2)
    // 0x8F=10001111, 0xF1=11110001, MSB first
    val expectedColors = Seq(3, 2, 2, 2, 1, 1, 1, 3).map(_.toUByte)
    assertEquals(ringBufferPixels.map(_.colorIndex), expectedColors)
  }

  test("PushStep does not push when FIFO is not empty") {
    val state = makeState(ly = 0)
    state.backgroundFetcher.tile.tileDataLow = UByte(0xFF)
    state.backgroundFetcher.tile.tileDataHigh = UByte(0x00)
    state.backgroundFifo.enqueue(BackgroundPixel.empty)
    state.backgroundFetcher.step = BackgroundFetcher.PushStep

    state.backgroundFetcher.step.tick(state)
    assertEquals(state.backgroundFetcher.step, BackgroundFetcher.PushStep) // advanced
    assertEquals(state.backgroundFifo.size, 1)
  }

  test("3-cycle GetTile -> Push") {
    val state = makeState(ly = 0)
    val cycleData = Seq(
      CycleData(UByte(0xAA), UByte(0x55), Seq(1, 2, 1, 2, 1, 2, 1, 2)),
      CycleData(UByte(0x33), UByte(0xCC), Seq(2, 2, 1, 1, 2, 2, 1, 1)),
      CycleData(UByte(0x0F), UByte(0xF0), Seq(2, 2, 2, 2, 1, 1, 1, 1))
    )

    cycleData.zipWithIndex.foreach { case (data, i) =>
      val tileNumber = i + 1
      placeTile(state, tileNumber = tileNumber, x = i)
      val addr = tileNumber * 16
      state.vram(addr) = data.low
      state.vram(addr + 1) = data.high
    }
    // Correctly initialize state.backgroundFetcher.tile to point to tile 1
    state.backgroundFetcher.tile.tileDataAddress = 16
    state.backgroundFetcher.step = BackgroundFetcher.GetTileStep

    val actualColorsSequence = (0 until 3).map { _ =>
      runFetcherStepCycle(state)
    }

    val expectedColorsSequence = cycleData.map(_.expectedColors.map(_.toUByte))

    assertEquals(actualColorsSequence, expectedColorsSequence)
    assertEquals(state.backgroundFetcher.fetcherX, 3)
    assertEquals(state.backgroundFifo.size, 0)
  }

  test("windowActive is false when windowEnable is false") {
    val state = makeState(windowEnable = false, wy = 0, ly = 0, wx = 7)
    state.backgroundFetcher.fetcherX = 0
    state.backgroundFetcher.step = BackgroundFetcher.GetTileStep
    tickBackgroundUntilStepChange(state)
    assertEquals(state.backgroundFetcher.windowActive, false)
  }

  test("windowActive is false when WY > LY") {
    val state = makeState(windowEnable = true, wy = 5, ly = 3, wx = 7)
    state.backgroundFetcher.fetcherX = 0
    state.backgroundFetcher.step = BackgroundFetcher.GetTileStep
    tickBackgroundUntilStepChange(state)
    assertEquals(state.backgroundFetcher.windowActive, false)
  }

  test("windowActive is false when fetcherX * 8 < WX - 7") {
    val state = makeState(windowEnable = true, wy = 0, ly = 0, wx = 20)
    state.backgroundFetcher.fetcherX = 0
    state.backgroundFetcher.step = BackgroundFetcher.GetTileStep
    tickBackgroundUntilStepChange(state)
    assertEquals(state.backgroundFetcher.windowActive, false)
  }

  test("windowActive is true when all conditions met") {
    val state = makeState(windowEnable = true, wy = 0, ly = 0, wx = 7)
    placeTile(state, tileNumber = 0)
    state.backgroundFetcher.fetcherX = 0
    state.backgroundFetcher.step = BackgroundFetcher.GetTileStep
    tickBackgroundUntilStepChange(state)
    assertEquals(state.backgroundFetcher.windowActive, true)
  }

  test("reset clears all state") {
    val state = makeState(ly = 0)
    state.backgroundFetcher.fetcherX = 5
    state.backgroundFetcher.dot = 1
    state.backgroundFetcher.windowActive = true
    state.backgroundFetcher.step = BackgroundFetcher.PushStep

    state.backgroundFetcher.reset()

    assertEquals(state.backgroundFetcher.fetcherX, 0)
    assertEquals(state.backgroundFetcher.dot, 0)
    assertEquals(state.backgroundFetcher.windowActive, false)
    assertEquals(state.backgroundFetcher.step, BackgroundFetcher.GetTileStep)
  }
}

object BackgroundFetcherTests extends BackgroundFetcherFixtures
