package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.{GameboyObject, ObjectPixel, Ppu, Tile}
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Object Fetcher - Produces pixels from the OAM scan buffer
 *
 * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
 */
object ObjectFetcher {

  /**
   * Object fetch state: the step in the 8-dot fetch sequence, the reused decoded row,
   * and the cursors naming the object in flight and the next one due.
   *
   * @param fetchingObjectIndex Object index the in-flight fetch is reading.
   * @param scanlineObjectIndex Cursor into the sorted scanline objects.
   * @see [[https://gbdev.io/pandocs/Rendering.html#obj-penalty-algorithm]]
   */
  case class State(
    pixels: Array[ObjectPixel] = Array.fill(Tile.SIZE)(ObjectPixel.empty),
    var step: PixelFetcher.Step = ObjectFetcher.GetTileStep,
    private[fetcher] var dot: Int = 0,
    private[fetcher] var fetchingObjectIndex: Int = 0,
    private[ppu] var isFetching: Boolean = false,
    private[ppu] var scanlineObjectIndex: Int = 0,
    override private[fetcher] val tile: Tile = Tile(),
  ) extends PixelFetcher.State {

    def startFetch(index: Int): Unit = {
      this.fetchingObjectIndex = index
      this.dot = 0
      step = ObjectFetcher.GetTileStep
      isFetching = true
    }

    /**
     * A cancelled fetch never reaches the FIFO, so its row is discarded.
     *
     * @see [[https://gbdev.io/pandocs/pixel_fifo.html#object-fetch-canceling]]
     */
    def cancel(): Unit = isFetching = false

    def beginScanline(): Unit = reset()

    override private[fetcher] def reset(): Unit = {
      step = ObjectFetcher.GetTileStep
      dot = 0
      fetchingObjectIndex = 0
      scanlineObjectIndex = 0
      isFetching = false
    }
  }

  sealed trait Step extends PixelFetcher.Step {
    final protected def fetcherOf(ppuState: Ppu.State): PixelFetcher.State = ppuState.objectFetcher
  }

  sealed abstract class TwoDotStep extends PixelFetcher.TwoDotStep with Step

  /**
   * Get Tile - Resolves the object's tile row straight from its OAM entry
   *
   * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
   */
  case object GetTileStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.objectFetcher
      fetcher.tile.resolveFromOam(ppuState, ppuState.scanlineObjects(fetcher.fetchingObjectIndex))
      GetTileDataLowStep
    }
  }

  /**
   * Get Tile Data Low - Pulls data for the resolved object tile
   *
   * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
   */
  case object GetTileDataLowStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.objectFetcher
      fetcher.tile.tileDataLow = ppuState.vram(fetcher.tile.tileDataAddress)
      GetTileDataHighStep
    }
  }

  /**
   * Get Tile Data High - Pulls data for the resolved object tile
   *
   * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
   */
  case object GetTileDataHighStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.objectFetcher
      fetcher.tile.tileDataHigh = ppuState.vram(fetcher.tile.tileDataAddress + 1)
      PushStep
    }
  }

  /**
   * Push - Merges the fetched object row into the object FIFO, then ends the fetch
   *
   * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
   */
  case object PushStep extends Step {

    override protected def stepTick(ppuState: Ppu.State, fetcher: PixelFetcher.State): Step = {
      val objectFetcher = ppuState.objectFetcher
      val obj = ppuState.scanlineObjects(objectFetcher.fetchingObjectIndex)
      writePixelsFromOamRow(objectFetcher, obj)
      mergeIntoFifo(ppuState, objectFetcher, objectPixelForFifo(obj, ppuState))
      objectFetcher.isFetching = false
      GetTileStep
    }

    /**
     * Which of the object's 8 pixels is due, `shiftPosition - leftEdge`, clamped at 0 so
     * an object off the left edge drops its whole row.
     *
     * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
     */
    private def objectPixelForFifo(obj: GameboyObject, state: Ppu.State): Int =
      math.max(0, state.pixelMixer.shiftPosition - obj.leftEdge)

    /**
     * Decode the tile row into object pixels, applying horizontal flip.
     *
     * @see [[https://gbdev.io/pandocs/OAM.html#byte-1--x-position]]
     * @see [[https://gbdev.io/pandocs/OAM.html#byte-3--attributesflags]]
     */
    private def writePixelsFromOamRow(fetcher: ObjectFetcher.State, obj: GameboyObject): Unit = {
      val low = fetcher.tile.tileDataLow.toInt
      val high = fetcher.tile.tileDataHigh.toInt
      var i = 0
      while (i < Tile.SIZE) {
        val bit = if (obj.xFlipped) i else Tile.SIZE - 1 - i
        val pixel = fetcher.pixels(i)
        pixel.colorIndex = ((((high >> bit) & 1) << 1) | ((low >> bit) & 1)).toUByte
        pixel.usePalette0 = obj.usePalette0
        pixel.backgroundPriority = obj.backgroundPriority
        i += 1
      }
    }

    /**
     * Merge the decoded row into the object FIFO, offset by [[objectPixelForFifo]] and
     * padded to a full tile. An opaque pixel from an earlier object is left alone, so
     * the object fetched first wins the overlap.
     *
     * @see [[https://gbdev.io/pandocs/OAM.html#drawing-priority]]
     * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
     */
    private def mergeIntoFifo(
      state: Ppu.State,
      fetcher: ObjectFetcher.State,
      objectPixelForFifo: Int
    ): Unit =
      state.objectFifo.fillFromHead { (current, occupied, slot) =>
        val objectPixelIndex = slot + objectPixelForFifo
        if (occupied && current.isOpaque) current
        else if (objectPixelIndex < Tile.SIZE) fetcher.pixels(objectPixelIndex)
        else ObjectPixel.empty
      }
  }
}
