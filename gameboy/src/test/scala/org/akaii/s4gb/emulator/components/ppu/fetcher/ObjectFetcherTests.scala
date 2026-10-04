package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.*
import munit.FunSuite
import org.akaii.s4gb.extensions.byteops.toUByte
import spire.math.UByte

class ObjectFetcherTests extends FunSuite {

  import ObjectFetcherTests.*

  test("GetTileStep reads tile number and tileDataAddress, transitions to GetTileDataLowStep") {
    val state = makeState(ly = 3)
    placeObject(state, y = 16, tileIndex = TEST_OBJECT_TILE_NUMBER)
    state.objectFetcher.startFetch(0)

    val (next, dots) = tickObjectUntilStepChange(state)
    assertEquals(next, ObjectFetcher.GetTileDataLowStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.objectFetcher.tile.tileNumber, TEST_OBJECT_TILE_NUMBER)
    // 8 * 2 * 5 + 3 * 2 = 86
    assertEquals(state.objectFetcher.tile.tileDataAddress, 86)
  }

  test("GetTileStep applies Y flip to the row address") {
    val state = makeState(ly = 3)
    placeObject(state, y = 16, tileIndex = TEST_OBJECT_TILE_NUMBER, attributes = OAM_Y_FLIP)
    state.objectFetcher.startFetch(0)

    tickObjectUntilStepChange(state)
    // row 3 of 8 flipped is row 4: 5 * 16 + 4 * 2 = 88
    assertEquals(state.objectFetcher.tile.tileDataAddress, 88)
  }

  test("GetTileStep selects the top or bottom tile of an 8x16 object") {
    val state = makeState(ly = 9, objSize = true)
    placeObject(state, y = 16, tileIndex = 6)
    state.objectFetcher.startFetch(0)

    tickObjectUntilStepChange(state)
    // low bit of the OAM tile index is ignored, row 9 picks the bottom tile
    assertEquals(state.objectFetcher.tile.tileNumber, 7)
    assertEquals(state.objectFetcher.tile.tileDataAddress, 7 * 16 + 1 * 2)
  }

  test("GetTileDataLowStep reads low byte, transitions to GetTileDataHighStep") {
    val state = makeState(ly = 0)
    val expectedTileDataLow = UByte(0xAB)
    state.objectFetcher.tile.tileDataAddress = 100
    state.vram(100) = expectedTileDataLow
    state.objectFetcher.step = ObjectFetcher.GetTileDataLowStep

    val (next, dots) = tickObjectUntilStepChange(state)
    assertEquals(next, ObjectFetcher.GetTileDataHighStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.objectFetcher.tile.tileDataLow, expectedTileDataLow)
  }

  test("GetTileDataHighStep reads high byte, transitions to PushStep") {
    val state = makeState(ly = 0)
    val expectedTileDataHigh = UByte(0xCD)
    state.objectFetcher.tile.tileDataAddress = 100
    state.vram(101) = expectedTileDataHigh
    state.objectFetcher.step = ObjectFetcher.GetTileDataHighStep

    val (next, dots) = tickObjectUntilStepChange(state)
    assertEquals(next, ObjectFetcher.PushStep)
    assertEquals(dots, PixelFetcher.TWO_DOT_MAX)
    assertEquals(state.objectFetcher.tile.tileDataHigh, expectedTileDataHigh)
  }

  test("PushStep extracts 8 pixels MSB first and ends the fetch") {
    val state = makeState(ly = 0)
    placeObject(state, tileIndex = 0)
    state.objectFetcher.tile.tileDataLow = UByte(0x8F)
    state.objectFetcher.tile.tileDataHigh = UByte(0xF1)
    state.objectFetcher.step = ObjectFetcher.PushStep

    state.objectFetcher.step.tick(state)
    assertEquals(state.objectFetcher.step, ObjectFetcher.GetTileStep)
    assertEquals(state.objectFetcher.isFetching, false)
    assertEquals(state.objectFifo.size, Tile.SIZE)
    // ring buffer pixels and fetcher pixels must be distinct references
    val ringBufferSink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(ringBufferSink), Tile.SIZE)
    assertEquals(state.objectFifo.size, 0)
    val ringBufferPixels = ringBufferSink.toSeq
    val fetcherPixels = state.objectFetcher.pixels.toSeq
    val allIdentities = (ringBufferPixels ++ fetcherPixels).map(System.identityHashCode(_))
    assertEquals(allIdentities.toSet.size, Tile.SIZE * 2)
    // 0x8F=10001111, 0xF1=11110001, MSB first
    val expectedColors = Seq(3, 2, 2, 2, 1, 1, 1, 3).map(_.toUByte)
    assertEquals(ringBufferPixels.map(_.colorIndex), expectedColors)
  }

  test("PushStep carries the OAM palette and background priority onto every pixel") {
    val state = makeState(ly = 0)
    placeObject(state, tileIndex = 0, attributes = OAM_PALETTE_1 | OAM_BG_PRIORITY)
    state.objectFetcher.tile.tileDataLow = UByte(0xFF)
    state.objectFetcher.tile.tileDataHigh = UByte(0x00)
    state.objectFetcher.step = ObjectFetcher.PushStep

    state.objectFetcher.step.tick(state)

    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(sink), Tile.SIZE)
    sink.foreach { pixel =>
      assertEquals(pixel.usePalette0, false)
      assertEquals(pixel.backgroundPriority, true)
      assertEquals(pixel.colorIndex, 1.toUByte)
    }
  }

  test("PushStep decodes LSB first when the object is X flipped") {
    val state = makeState(ly = 0)
    placeObject(state, tileIndex = 0, attributes = OAM_X_FLIP)
    state.objectFetcher.tile.tileDataLow = UByte(0x8F)
    state.objectFetcher.tile.tileDataHigh = UByte(0xF1)
    state.objectFetcher.step = ObjectFetcher.PushStep

    state.objectFetcher.step.tick(state)

    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(sink), Tile.SIZE)
    // mirror of the MSB first order: 0x8F=10001111, 0xF1=11110001
    val expectedColors = Seq(3, 1, 1, 1, 2, 2, 2, 3).map(_.toUByte)
    assertEquals(sink.toSeq.map(_.colorIndex), expectedColors)
  }

  test("PushStep loads the rightmost 7 pixels when OAM X is below 8") {
    val state = makeState(ly = 0)
    placeObject(state, tileIndex = 0, x = 7)
    state.objectFetcher.tile.tileDataLow = UByte(0xFF)
    state.objectFetcher.tile.tileDataHigh = UByte(0xFF)
    state.objectFetcher.step = ObjectFetcher.PushStep

    state.objectFetcher.step.tick(state)

    assertEquals(state.objectFifo.size, Tile.SIZE)
    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(sink), Tile.SIZE)
    // left edge sits at x -1, so the shifter is already one column along the row
    sink.take(Tile.SIZE - 1).foreach(pixel => assertEquals(pixel.colorIndex, 3.toUByte))
    assertEquals(sink.last.colorIndex, ObjectPixel.transparentColor)
  }

  test("PushStep drops the pixels the shifter has already passed when the fetch starts late") {
    val state = makeState(ly = 0)
    placeObject(state, tileIndex = 0, x = 8)
    state.objectFetcher.tile.tileDataLow = UByte(0xFF)
    state.objectFetcher.tile.tileDataHigh = UByte(0xF0)
    state.objectFetcher.step = ObjectFetcher.PushStep
    state.pixelMixer.shiftPosition = 3

    state.objectFetcher.step.tick(state)

    // 0xFF / 0xF0 is 3,3,3,3,1,1,1,1 MSB first, so the row as loaded starts on pixel 3
    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(sink), Tile.SIZE)
    assertEquals(sink.toSeq.map(_.colorIndex), Seq(3, 1, 1, 1, 1, 0, 0, 0).map(_.toUByte))
  }

  test("PushStep loads the whole row when the object starts right of the shifter") {
    val state = makeState(ly = 0)
    placeObject(state, tileIndex = 0, x = 16)
    state.objectFetcher.tile.tileDataLow = UByte(0xFF)
    state.objectFetcher.tile.tileDataHigh = UByte(0xFF)
    state.objectFetcher.step = ObjectFetcher.PushStep
    state.pixelMixer.shiftPosition = 0

    state.objectFetcher.step.tick(state)

    // leftEdge is 8, so the row would start behind the head of the FIFO and is clamped to 0
    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(sink), Tile.SIZE)
    sink.foreach(pixel => assertEquals(pixel.colorIndex, 3.toUByte))
  }

  test("PushStep pads the object FIFO out to a full tile") {
    val state = makeState(ly = 0)
    placeObject(state, tileIndex = 0, x = 8)
    state.objectFetcher.tile.tileDataLow = UByte(0x00)
    state.objectFetcher.tile.tileDataHigh = UByte(0x00)
    state.objectFetcher.step = ObjectFetcher.PushStep

    assertEquals(state.objectFifo.size, 0)
    state.objectFetcher.step.tick(state)

    assertEquals(state.objectFifo.size, Tile.SIZE)
    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(sink), Tile.SIZE)
    sink.foreach(pixel => assertEquals(pixel.colorIndex, ObjectPixel.transparentColor))
  }

  test("PushStep preserves an opaque pixel already held in the object FIFO") {
    val state = makeState(ly = 0)
    val heldPixel = ObjectPixel(colorIndex = 3.toUByte, usePalette0 = false, backgroundPriority = true)
    state.objectFifo.enqueue(heldPixel)
    placeObject(state, tileIndex = 0, x = 8)
    state.objectFetcher.tile.tileDataLow = UByte(0xFF)
    state.objectFetcher.tile.tileDataHigh = UByte(0x00)
    state.objectFetcher.step = ObjectFetcher.PushStep

    state.objectFetcher.step.tick(state)

    assertEquals(state.objectFifo.size, Tile.SIZE)
    val sink = Array.fill(Tile.SIZE)(ObjectPixel.empty)
    assertEquals(state.objectFifo.dequeue(sink), Tile.SIZE)
    assertEquals(sink.head.colorIndex, 3.toUByte)
    assertEquals(sink.head.usePalette0, false)
    assertEquals(sink.head.backgroundPriority, true)
    sink.tail.foreach { pixel =>
      assertEquals(pixel.colorIndex, 1.toUByte)
      assertEquals(pixel.usePalette0, true)
      assertEquals(pixel.backgroundPriority, false)
    }
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
      val addr = tileNumber * 16
      state.vram(addr) = data.low
      state.vram(addr + 1) = data.high
      placeObject(state, index = i, tileIndex = tileNumber)
    }

    val actualColorsSequence = cycleData.indices.map { i =>
      state.objectFetcher.startFetch(i)
      runObjectFetchStepCycle(state).map(_.colorIndex)
    }

    assertEquals(actualColorsSequence, cycleData.map(_.expectedColors.map(_.toUByte)))
    assertEquals(state.objectFifo.size, 0)
  }

  test("startFetch begins a fetch") {
    val state = makeState()
    assertEquals(state.objectFetcher.isFetching, false)

    state.objectFetcher.startFetch(4)
    assertEquals(state.objectFetcher.isFetching, true)
    assertEquals(state.objectFetcher.step, ObjectFetcher.GetTileStep)
    assertEquals(state.objectFetcher.fetchingObjectIndex, 4)
    assertEquals(state.objectFetcher.dot, 0)
  }

  test("cancel abandons the fetch without rewinding the sequence") {
    val state = makeState()
    state.objectFetcher.startFetch(2)
    state.objectFetcher.step = ObjectFetcher.PushStep
    state.objectFetcher.dot = 1

    state.objectFetcher.cancel()

    assertEquals(state.objectFetcher.isFetching, false)
    assertEquals(state.objectFetcher.step, ObjectFetcher.PushStep)
    assertEquals(state.objectFetcher.dot, 1)
  }

  test("reset clears all state") {
    val state = makeState()
    state.objectFetcher.startFetch(7)
    state.objectFetcher.dot = 1
    state.objectFetcher.scanlineObjectIndex = 3

    state.objectFetcher.reset()

    assertEquals(state.objectFetcher.isFetching, false)
    assertEquals(state.objectFetcher.step, ObjectFetcher.GetTileStep)
    assertEquals(state.objectFetcher.fetchingObjectIndex, 0)
    assertEquals(state.objectFetcher.dot, 0)
    assertEquals(state.objectFetcher.scanlineObjectIndex, 0)
  }
}

object ObjectFetcherTests extends ObjectFetcherFixtures
