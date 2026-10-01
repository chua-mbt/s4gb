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

  case class State(
    var step: PixelFetcher.Step = ObjectFetcher.GetTileStep,
    pixels: Array[ObjectPixel] = Array.fill(Tile.SIZE)(ObjectPixel.empty),
    private[fetcher] var dot: Int = 0,
    private[fetcher] var objectIndex: Int = 0,
    private[fetcher] var isFetching: Boolean = false,
    override private[fetcher] val tile: Tile = Tile(),
  ) extends PixelFetcher.State {
    def startFetch(objectIndex: Int): Unit = {
      this.objectIndex = objectIndex
      this.dot = 0
      step = ObjectFetcher.GetTileStep
      isFetching = true
    }

    override private[fetcher] def reset(): Unit = {
      step = ObjectFetcher.GetTileStep
      dot = 0
      objectIndex = 0
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
      fetcher.tile.resolveFromOam(ppuState, ppuState.scanlineObjects(fetcher.objectIndex))
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
      decodeRow(objectFetcher, ppuState.scanlineObjects(objectFetcher.objectIndex))
      mergeIntoFifo(ppuState, objectFetcher)
      objectFetcher.isFetching = false
      GetTileStep
    }

    /**
     * Decode the tile row into object pixels, applying horizontal flip.
     *
     * Pixels that land left of screen x=0 are left transparent so the merge
     * skips them, which is how an object with an OAM X below 8 is clipped.
     *
     * @see [[https://gbdev.io/pandocs/OAM.html#byte-1--x-position]]
     * @see [[https://gbdev.io/pandocs/OAM.html#byte-3--attributesflags]]
     */
    @inline private def decodeRow(fetcher: ObjectFetcher.State, obj: GameboyObject): Unit = {
      val low = fetcher.tile.tileDataLow.toInt
      val high = fetcher.tile.tileDataHigh.toInt
      val clipped = math.max(0, Tile.SIZE - obj.x.toInt)
      var i = 0
      while (i < Tile.SIZE) {
        val bit = if (obj.xFlipped) i else Tile.SIZE - 1 - i
        val pixel = fetcher.pixels(i)
        pixel.colorIndex =
          if (i < clipped) ObjectPixel.transparentColor
          else ((((high >> bit) & 1) << 1) | ((low >> bit) & 1)).toUByte
        pixel.usePalette0 = obj.usePalette0
        pixel.backgroundPriority = obj.backgroundPriority
        i += 1
      }
    }

    /**
     * Merge the decoded row into the first eight slots of the object FIFO.
     *
     * The FIFO is padded out to a full tile of transparent pixels first so that
     * the slots always exist, and an opaque pixel already held there by an
     * earlier object is never overwritten. The object fetched first therefore
     * wins any overlap, and the padding is what makes the FIFO usable by the
     * mixer when no object pixel lands on a given column.
     *
     * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
     */
    @inline private def mergeIntoFifo(state: Ppu.State, fetcher: ObjectFetcher.State): Unit =
      state.objectFifo.fillFromHead { (current, occupied, offset) =>
        if (occupied && current.isOpaque) current
        else fetcher.pixels(offset)
      }
  }
}
