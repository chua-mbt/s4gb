package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.{BackgroundPixel, Ppu, Tile}
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Background Pixel Fetcher - Produces pixels from background and window only
 *
 * Pan Docs lists a Sleep step between Get Tile Data High and Push. No evidence for
 * it was found in GBEDG, SameBoy or Gambatte, so the fetch sequence here omits it.
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html#fifo-pixel-fetcher]]
 * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#background-pixel-fetching]]
 */
object BackgroundFetcher {

  private val WX_OFFSET: Int = 7 // WX is "X position plus 7" per pandocs; WX=7 places window at screen pixel 0

  /**
   * Background fetch state: the step in the fetch sequence, the reused decoded row, and
   * the tile map position reached so far.
   *
   * @param fetcherX Tile column fetched so far, restarts at 0 for the window.
   * @param windowActive Set once the window is ready, for the rest of the line.
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#fifo-pixel-fetcher]]
   */
  case class State(
    pixels: Array[BackgroundPixel] = Array.fill(Tile.SIZE)(BackgroundPixel.empty),
    var step: PixelFetcher.Step = BackgroundFetcher.GetTileStep,
    var windowRowsRendered: Int = 0,
    private[fetcher] var dot: Int = 0,
    private[fetcher] var fetcherX: Int = 0,
    private[fetcher] var windowActive: Boolean = false,
    override private[fetcher] val tile: Tile = Tile(),
  ) extends PixelFetcher.State {
    def resetWindowRowsRendered(): Unit = windowRowsRendered = 0

    def beginScanline(): Unit = {
      if (windowActive) advanceWindowRowsRendered()
      reset()
    }

    private[fetcher] def advanceWindowRowsRendered(): Unit = windowRowsRendered += 1

    /**
     * An object fetch resets the fetcher to step 1 and pauses it. Only the sequence rewinds,
     * the tile map position is counted in fetches so it survives, and the window stays on.
     *
     * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
     */
    private[ppu] def restartForObjectFetch(): Unit = {
      step = BackgroundFetcher.GetTileStep
      dot = 0
    }

    /**
     * Hand over to the window when due, restarting the fetch sequence on its top left tile.
     * Once on it stays on for the rest of the line. Triggered by the shifter position,
     * since WX names an on-screen pixel, less the 7 pixels Pan Docs counts before the
     * first is rendered.
     *
     * @see [[https://gbdev.io/pandocs/Window.html#window-rendering-criteria]]
     * @see [[https://gbdev.io/pandocs/LCDC.html#lcdc5--window-enable]]
     */
    private[ppu] def startWindow(state: Ppu.State): Unit =
      if (!windowActive && windowReady(state)) {
        windowActive = true
        step = BackgroundFetcher.GetTileStep
        dot = 0
        fetcherX = 0
      }

    private def windowReady(state: Ppu.State): Boolean =
      state.lcdControl.windowEnable &&
        state.lcdControl.bgEnable &&
        state.registers(Ppu.Address.WY).toInt <= state.ly.toInt &&
        state.pixelMixer.shiftPosition >= state.registers(Ppu.Address.WX).toInt - WX_OFFSET

    override private[fetcher] def reset(): Unit = {
      step = BackgroundFetcher.GetTileStep
      dot = 0
      fetcherX = 0
      windowActive = false
    }
  }

  sealed trait Step extends PixelFetcher.Step {
    final protected def fetcherOf(ppuState: Ppu.State): PixelFetcher.State = ppuState.backgroundFetcher
  }

  sealed abstract class TwoDotStep extends PixelFetcher.TwoDotStep with Step

  /**
   * Get Tile - Finds reference to relevant tile
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#get-tile]]
   */
  case object GetTileStep extends TwoDotStep {
    override def finalTick(ppuState: Ppu.State): Step = {
      val fetcher = ppuState.backgroundFetcher
      fetcher.tile.resolveFromTileMap(ppuState, fetcher.fetcherX, fetcher.windowRowsRendered, fetcher.windowActive)
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
      val fetcher = ppuState.backgroundFetcher
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
      val fetcher = ppuState.backgroundFetcher
      fetcher.tile.tileDataHigh = ppuState.vram(fetcher.tile.tileDataAddress + 1)
      PushStep
    }
  }

  /**
   * Push - Pushes all pixels from the tile
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#push]]
   * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#background-pixel-fetching]]
   */
  case object PushStep extends Step {

    override protected def stepTick(ppuState: Ppu.State, fetcher: PixelFetcher.State): Step =
      if (ppuState.backgroundFifo.isEmpty) {
        val background = ppuState.backgroundFetcher
        writePixelsFromTileRow(background)
        ppuState.backgroundFifo.enqueueAll(background.pixels)
        background.fetcherX += 1
        GetTileStep
      } else {
        this
      }

    private def writePixelsFromTileRow(fetcher: BackgroundFetcher.State): Unit = {
      val low = fetcher.tile.tileDataLow.toInt
      val high = fetcher.tile.tileDataHigh.toInt
      var i = 0
      while (i < Tile.SIZE) {
        val bit = Tile.SIZE - 1 - i
        val colorIndex = ((high >> bit) & 1) << 1 | ((low >> bit) & 1)
        fetcher.pixels(i).colorIndex = colorIndex.toUByte
        i += 1
      }
    }
  }
}
