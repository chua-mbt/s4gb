package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.{Ppu, Tile}

/**
 * Shared state and state machine of the pixel fetchers, which differ only in
 * what they resolve and where they push:
 *
 * - [[BackgroundFetcher]] reads a tile map and enqueues onto the background FIFO
 * - [[ObjectFetcher]] reads the OAM scan buffer and merges into the object FIFO
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html#fifo-pixel-fetcher]]
 */
object PixelFetcher {

  private[fetcher] val TWO_DOT_MAX: Int = 2

  trait State {

    var step: Step
    private[fetcher] var dot: Int
    private[fetcher] val tile: Tile

    private[fetcher] def reset(): Unit
  }

  trait Step {

    protected def fetcherOf(ppuState: Ppu.State): State

    final def tick(ppuState: Ppu.State): Unit = {
      val fetcher = fetcherOf(ppuState)
      fetcher.step = stepTick(ppuState, fetcher)
    }

    protected def stepTick(ppuState: Ppu.State, fetcher: State): Step
  }

  abstract class TwoDotStep extends Step {

    /**
     * Step-specific behaviours on final tick
     */
    def finalTick(ppuState: Ppu.State): Step

    /**
     * Each step takes 2 dots: address on dot 1, data latch on dot 2.
     * VRAM doesn't mutate during Mode 3 (CPU writes blocked), so we just act on the read on the final dot.
     */
    override protected def stepTick(ppuState: Ppu.State, fetcher: State): Step = {
      fetcher.dot += 1
      if (fetcher.dot >= TWO_DOT_MAX) {
        fetcher.dot = 0
        finalTick(ppuState)
      } else {
        this
      }
    }
  }
}
