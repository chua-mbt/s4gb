package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.extensions.byteops.toUByte
import spire.math.UByte

class PixelFetcherTests extends FunSuite {

  import PixelFetcherTests.*

  test("GetTileStep reads tile number and tileDataAddress, transitions to GetTileDataLowStep") {
    val state = makeState(ly = 3)
    placeTile(state, tileNumber = TEST_TILE_NUMBER)
    state.pixelFetcher.step = PixelFetcher.GetTileStep

    val (next, dots) = tickUntilStepChange(state)
    assertEquals(next, PixelFetcher.GetTileDataLowStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.pixelFetcher.tile.tileNumber, TEST_TILE_NUMBER)
    // 8 * 2 * 5 + 3 * 2 = 86
    assertEquals(state.pixelFetcher.tile.tileDataAddress, 86)
  }

  test("GetTileDataLowStep reads low byte, transitions to GetTileDataHighStep") {
    val state = makeState(ly = 0)
    val expectedTileDataLow = UByte(0xAB)
    state.pixelFetcher.tile.tileDataAddress = 100
    state.vram(100) = expectedTileDataLow
    state.pixelFetcher.step = PixelFetcher.GetTileDataLowStep

    val (next, dots) = tickUntilStepChange(state)
    assertEquals(next, PixelFetcher.GetTileDataHighStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.pixelFetcher.tile.tileDataLow, expectedTileDataLow)
  }

  test("GetTileDataHighStep reads high byte, transitions to SleepStep") {
    val state = makeState(ly = 0)
    val expectedTileDataHigh = UByte(0xCD)
    state.pixelFetcher.tile.tileDataAddress = 100
    state.vram(101) = expectedTileDataHigh
    state.pixelFetcher.step = PixelFetcher.GetTileDataHighStep

    val (next, dots) = tickUntilStepChange(state)
    assertEquals(next, PixelFetcher.SleepStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.pixelFetcher.tile.tileDataHigh, expectedTileDataHigh)
  }

  test("SleepStep transitions to PushStep without changing state") {
    val state = makeState()
    state.pixelFetcher.fetcherX = 3
    state.pixelFetcher.tile.tileNumber = 7
    state.pixelFetcher.tile.tileDataLow = UByte(0xAB)
    state.pixelFetcher.tile.tileDataHigh = UByte(0xCD)
    state.pixelFetcher.step = PixelFetcher.SleepStep

    val (next, dots) = tickUntilStepChange(state)
    assertEquals(next, PixelFetcher.PushStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)

    // unchanged
    assertEquals(state.pixelFetcher.fetcherX, 3)
    assertEquals(state.pixelFetcher.tile.tileNumber, 7)
    assertEquals(state.pixelFetcher.tile.tileDataLow, UByte(0xAB))
    assertEquals(state.pixelFetcher.tile.tileDataHigh, UByte(0xCD))
  }

  test("PushStep extracts 8 pixels MSB first, advances fetcherX, transitions to GetTileStep") {
    val state = makeState(ly = 0)
    state.pixelFetcher.tile.tileDataLow = UByte(0x8F)
    state.pixelFetcher.tile.tileDataHigh = UByte(0xF1)
    state.pixelFetcher.fetcherX = 0
    state.pixelFetcher.step = PixelFetcher.PushStep

    state.pixelFetcher.step.tick(state)
    assertEquals(state.pixelFetcher.step, PixelFetcher.GetTileStep) // advanced
    assertEquals(state.pixelFetcher.fetcherX, 1)
    assertEquals(state.backgroundFifo.size, Tile.SIZE)
    // ring buffer pixels and fetcher pixels must be distinct references
    val ringBufferSink = Array.fill(Tile.SIZE)(Pixel(Pixel.BG_PALETTE))
    assertEquals(state.backgroundFifo.dequeue(ringBufferSink), Tile.SIZE)
    assertEquals(state.backgroundFifo.size, 0)
    val ringBufferPixels = ringBufferSink.toSeq
    val fetcherPixels = state.pixelFetcher.pixels.toSeq
    val allIdentities = (ringBufferPixels ++ fetcherPixels).map(System.identityHashCode(_))
    assertEquals(allIdentities.toSet.size, Tile.SIZE * 2)
    // 0x8F=10001111, 0xF1=11110001, MSB first
    val expectedColors = Seq(3, 2, 2, 2, 1, 1, 1, 3).map(_.toUByte)
    assertEquals(ringBufferPixels.map(_.colorIndex), expectedColors)
  }

  test("PushStep does not push when FIFO is not empty") {
    val state = makeState(ly = 0)
    state.pixelFetcher.tile.tileDataLow = UByte(0xFF)
    state.pixelFetcher.tile.tileDataHigh = UByte(0x00)
    state.backgroundFifo.enqueue(Pixel(UByte(0)))
    state.pixelFetcher.step = PixelFetcher.PushStep

    state.pixelFetcher.step.tick(state)
    assertEquals(state.pixelFetcher.step, PixelFetcher.PushStep) // advanced
    assertEquals(state.backgroundFifo.size, 1)
  }

  case class TileCycleData(low: UByte, high: UByte, expectedColors: Seq[Int])

  test("3-cycle GetTile -> Push") {
    val state = makeState(ly = 0)
    val cycleData = Seq(
      TileCycleData(UByte(0xAA), UByte(0x55), Seq(1, 2, 1, 2, 1, 2, 1, 2)),
      TileCycleData(UByte(0x33), UByte(0xCC), Seq(2, 2, 1, 1, 2, 2, 1, 1)),
      TileCycleData(UByte(0x0F), UByte(0xF0), Seq(2, 2, 2, 2, 1, 1, 1, 1))
    )

    cycleData.zipWithIndex.foreach { case (data, i) =>
      val tileNumber = i + 1
      placeTile(state, tileNumber = tileNumber, x = i)
      val addr = tileNumber * 16
      state.vram(addr) = data.low
      state.vram(addr + 1) = data.high
    }
    // Correctly initialize state.pixelFetcher.tile to point to tile 1
    state.pixelFetcher.tile.tileDataAddress = 16
    state.pixelFetcher.step = PixelFetcher.GetTileStep

    val actualColorsSequence = (0 until 3).map { _ =>
      runFetcherStepCycle(state)
    }

    val expectedColorsSequence = cycleData.map(_.expectedColors.map(_.toUByte))

    assertEquals(actualColorsSequence, expectedColorsSequence)
    assertEquals(state.pixelFetcher.fetcherX, 3)
    assertEquals(state.backgroundFifo.size, 0)
  }

  test("windowActive is false when windowEnable is false") {
    val state = makeState(windowEnable = false, wy = 0, ly = 0, wx = 7)
    state.pixelFetcher.fetcherX = 0
    state.pixelFetcher.step = PixelFetcher.GetTileStep
    tickUntilStepChange(state)
    assertEquals(state.pixelFetcher.windowActive, false)
  }

  test("windowActive is false when WY > LY") {
    val state = makeState(windowEnable = true, wy = 5, ly = 3, wx = 7)
    state.pixelFetcher.fetcherX = 0
    state.pixelFetcher.step = PixelFetcher.GetTileStep
    tickUntilStepChange(state)
    assertEquals(state.pixelFetcher.windowActive, false)
  }

  test("windowActive is false when fetcherX * 8 < WX - 7") {
    val state = makeState(windowEnable = true, wy = 0, ly = 0, wx = 20)
    state.pixelFetcher.fetcherX = 0
    state.pixelFetcher.step = PixelFetcher.GetTileStep
    tickUntilStepChange(state)
    assertEquals(state.pixelFetcher.windowActive, false)
  }

  test("windowActive is true when all conditions met") {
    val state = makeState(windowEnable = true, wy = 0, ly = 0, wx = 7)
    placeTile(state, tileNumber = 0)
    state.pixelFetcher.fetcherX = 0
    state.pixelFetcher.step = PixelFetcher.GetTileStep
    tickUntilStepChange(state)
    assertEquals(state.pixelFetcher.windowActive, true)
  }

  test("reset clears all state") {
    val state = makeState(ly = 0)
    state.pixelFetcher.fetcherX = 5
    state.pixelFetcher.dot = 1
    state.pixelFetcher.windowActive = true
    state.pixelFetcher.step = PixelFetcher.PushStep

    state.pixelFetcher.reset()

    assertEquals(state.pixelFetcher.fetcherX, 0)
    assertEquals(state.pixelFetcher.dot, 0)
    assertEquals(state.pixelFetcher.windowActive, false)
    assertEquals(state.pixelFetcher.step, PixelFetcher.GetTileStep)
  }
}

object PixelFetcherTests extends PixelFetcherFixtures
