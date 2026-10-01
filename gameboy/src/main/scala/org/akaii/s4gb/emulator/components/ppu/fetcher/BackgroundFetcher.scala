package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.{BackgroundPixel, Ppu, Tile}
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Background Pixel Fetcher - Produces pixels from background and window only
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html#fifo-pixel-fetcher]]
 * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#background-pixel-fetching]]
 */
object BackgroundFetcher {

  private val WX_OFFSET: Int = 7 // WX is "X position plus 7" per pandocs; WX=7 places window at screen pixel 0

  case class State(
    var step: PixelFetcher.Step = BackgroundFetcher.GetTileStep,
    var windowRowsRendered: Int = 0,
    pixels: Array[BackgroundPixel] = Array.fill(Tile.SIZE)(BackgroundPixel.empty),
    private[fetcher] var dot: Int = 0,
    private[fetcher] var fetcherX: Int = 0,
    private[fetcher] var windowActive: Boolean = false,
    override private[fetcher] val tile: Tile = Tile(),
  ) extends PixelFetcher.State {
    def resetWindowRowsRendered(): Unit = windowRowsRendered = 0

    private[fetcher] def advanceWindowRowsRendered(): Unit = windowRowsRendered += 1

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
      val lcdc = ppuState.lcdControl
      val wx = ppuState.registers(Ppu.Address.WX).toInt - WX_OFFSET
      val wy = ppuState.registers(Ppu.Address.WY).toInt
      val ly = ppuState.ly.toInt
      fetcher.windowActive = lcdc.windowEnable && (wy <= ly) && (fetcher.fetcherX * Tile.SIZE >= wx)
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

    override protected def stepTick(ppuState: Ppu.State, fetcher: PixelFetcher.State): Step =
      if (ppuState.backgroundFifo.isEmpty) {
        val background = ppuState.backgroundFetcher
        tileToPixels(background)
        ppuState.backgroundFifo.enqueueAll(background.pixels)
        background.fetcherX += 1
        GetTileStep
      } else {
        this
      }

    @inline private def tileToPixels(fetcher: BackgroundFetcher.State): Unit = {
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
