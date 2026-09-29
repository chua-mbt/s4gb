package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/**
 * FIFO Pixel Fetcher - Produces pixels from background and window only
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html#fifo-pixel-fetcher]]
 * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#background-pixel-fetching]]
 */
case class PixelFetcher(
  var step: PixelFetcher.Step = PixelFetcher.GetTileStep,
  var dot: Int = 0,
  var fetcherX: Int = 0,
  var windowActive: Boolean = false,
  var windowRowsRendered: Int = 0,
  tile: Tile = Tile(),
  pixels: Array[BackgroundPixel] = Array.fill(Tile.SIZE)(BackgroundPixel.empty),
) {
  def reset(): Unit = {
    step = PixelFetcher.GetTileStep
    dot = 0
    fetcherX = 0
    windowActive = false
  }

  def resetWindowRowsRendered(): Unit = windowRowsRendered = 0

  def advanceWindowRowsRendered(): Unit = windowRowsRendered += 1
}

object PixelFetcher {

  val WX_OFFSET: Int = 7 // WX is "X position plus 7" per pandocs; WX=7 places window at screen pixel 0
  val TWO_DOT_MAX: Int = 2

  sealed trait Step {
    final def tick(ppuState: Ppu.State): Unit =
      ppuState.pixelFetcher.step = stepTick(ppuState)

    protected def stepTick(ppuState: Ppu.State): Step
  }

  sealed abstract class TwoDotStep extends Step {

    /**
     * Step-specific behaviours on final tick
     */
    def finalTick(ppuState: Ppu.State): Step

    /**
     * Each step takes 2 dots: address on dot 1, data latch on dot 2.
     * VRAM doesn't mutate during Mode 3 (CPU writes blocked), so we just act on the read on the final dot.
     */
    override protected def stepTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.pixelFetcher
      fetcher.dot += 1
      if (fetcher.dot >= TWO_DOT_MAX) {
        fetcher.dot = 0
        val next = finalTick(ppuState)
        next
      } else {
        this
      }
    }
  }

  /**
   * Get Tile - Finds reference to relevant tile
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#get-tile]]
   */
  case object GetTileStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.pixelFetcher
      val lcdc = ppuState.lcdControl
      val wx = ppuState.registers(Ppu.Address.WX).toInt - WX_OFFSET
      val wy = ppuState.registers(Ppu.Address.WY).toInt
      val ly = ppuState.ly.toInt
      fetcher.windowActive = lcdc.windowEnable && (wy <= ly) && (fetcher.fetcherX * Tile.SIZE >= wx)
      fetcher.tile.resolve(ppuState, fetcher.fetcherX, fetcher.windowRowsRendered, fetcher.windowActive)
      GetTileDataLowStep
    }
  }

  /**
   * Get Tile Data Low - Pulls data for relevant tile
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#get-tile-data-low]]
   */
  case object GetTileDataLowStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.pixelFetcher
      fetcher.tile.tileDataLow = ppuState.vram(fetcher.tile.tileDataAddress)
      GetTileDataHighStep
    }
  }

  /**
   * Get Tile Data High - Pulls data for relevant tile
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#get-tile-data-high]]
   */
  case object GetTileDataHighStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.pixelFetcher
      fetcher.tile.tileDataHigh = ppuState.vram(fetcher.tile.tileDataAddress + 1)
      SleepStep
    }
  }

  /**
   * Sleep
   * https://gbdev.io/pandocs/pixel_fifo.html#get-tile
   */
  case object SleepStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = PushStep
  }

  /**
   * Push - Pushes all pixels from the tile
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#push]]
   * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#background-pixel-fetching]]
   */
  case object PushStep extends Step {

    override protected def stepTick(ppuState: Ppu.State): Step =
      if (ppuState.backgroundFifo.isEmpty) {
        val fetcher = ppuState.pixelFetcher
        tileToPixels(fetcher)
        ppuState.backgroundFifo.enqueueAll(fetcher.pixels)
        fetcher.fetcherX += 1
        GetTileStep
      } else {
        this
      }

    private def tileToPixels(fetcher: PixelFetcher): Unit = {
      val low = fetcher.tile.tileDataLow.toInt
      val high = fetcher.tile.tileDataHigh.toInt
      val bitMask = 1
      var i = 0
      while (i < Tile.SIZE) {
        val bit = Tile.SIZE - 1 - i
        val colorIndex = ((high >> bit) & bitMask) << 1 | ((low >> bit) & bitMask)
        fetcher.pixels(i).colorIndex = UByte(colorIndex)
        i += 1
      }
    }
  }
}
