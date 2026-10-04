package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * PPU Modes
 *
 * @see [[https://gbdev.io/pandocs/Rendering.html#highlight=mode#ppu-modes]]
 * @see [[https://gbdev.io/pandocs/Accessing_VRAM_and_OAM.html]]
 */
sealed abstract class PpuMode(val statValue: UByte, val canAccessVram: Boolean, val canAccessOam: Boolean) {
  def tick(state: Ppu.State, interrupts: Interrupts): PpuMode

  protected def interruptsAndTransition(target: PpuMode, state: Ppu.State, interrupts: Interrupts): PpuMode = {
    val vBlankEntry = target == PpuMode.VerticalBlank
    val hblankLStatInterrupt = target == PpuMode.HorizontalBlank && state.lcdStatus.mode0Select
    val vblankLStatInterrupt = vBlankEntry && state.lcdStatus.mode1Select
    val oamScanLStatInterrupt = target == PpuMode.OamScan && state.lcdStatus.mode2Select
    val lStatInterrupt = hblankLStatInterrupt || vblankLStatInterrupt || oamScanLStatInterrupt

    if (vBlankEntry) {
      state.backgroundFetcher.resetWindowRowsRendered()
      interrupts.request(Interrupts.Source.VBlank)
    }
    if (lStatInterrupt) interrupts.request(Interrupts.Source.LCDStat)
    target
  }

  /**
   * Scans here so object zero is read on the first dot of mode 2.
   *
   * @see [[https://gbdev.io/pandocs/Rendering.html#obj-penalty-algorithm]]
   */
  protected def beginOamScan(state: Ppu.State, interrupts: Interrupts): PpuMode = {
    OamScanner.scan(state)
    interruptsAndTransition(PpuMode.OamScan, state, interrupts)
  }
}

object PpuMode {
  val VISIBLE_SCANLINES_END: UByte = 143.toUByte
  val VBLANK_START_LY: UByte = 144.toUByte
  val FRAME_WRAP_LY: UByte = 0.toUByte

  case object HorizontalBlank extends PpuMode(UByte(0x00), canAccessVram = true, canAccessOam = true) {
    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode =
      state.ly match {
        case VBLANK_START_LY if state.scanlineDot.isBoundary =>
          interruptsAndTransition(VerticalBlank, state, interrupts)
        case _ if state.scanlineDot.isBoundary =>
          beginOamScan(state, interrupts)
        case _ =>
          HorizontalBlank
      }
  }

  case object VerticalBlank extends PpuMode(UByte(0x01), canAccessVram = true, canAccessOam = true) {
    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode =
      state.ly match {
        case FRAME_WRAP_LY if state.scanlineDot.isBoundary =>
          beginOamScan(state, interrupts)
        case _ =>
          VerticalBlank
      }
  }

  case object OamScan extends PpuMode(UByte(0x02), canAccessVram = true, canAccessOam = false) {
    val TOTAL_DOTS: Int = 80

    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode = {
      if (state.scanlineDot.current >= TOTAL_DOTS) {
        OamScanner.sortByDrawingPriority(state)
        beginMode3(state)
        interruptsAndTransition(Draw, state, interrupts)
      } else {
        OamScanner.scan(state)
        OamScan
      }
    }

    /**
     * Both FIFOs are cleared at the start of mode 3.
     *
     * @see [[https://gbdev.io/pandocs/pixel_fifo.html#mode-3-operation]]
     */
    private def beginMode3(state: Ppu.State): Unit = {
      state.backgroundFetcher.beginScanline()
      state.objectFetcher.beginScanline()
      state.resetFifos()
      state.pixelMixer.beginScanline(state)
    }
  }

  case object Draw extends PpuMode(UByte(0x03), canAccessVram = false, canAccessOam = false) {
    override def tick(state: Ppu.State, interrupts: Interrupts): PpuMode = {
      val renderPixel = advanceFetch(state)
      if (renderPixel) state.pixelMixer.tick(state, state.emitter)

      val scanlineCompleted = state.pixelMixer.renderedPixels >= Ppu.VISIBLE_WIDTH
      if (scanlineCompleted) interruptsAndTransition(HorizontalBlank, state, interrupts)
      else this
    }

    /**
     * An object fetch owns the dot: the background fetcher is reset and paused, and no
     * pixel is rendered while it runs.
     *
     * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
     */
    private def advanceFetch(state: Ppu.State): Boolean = {
      val fetcher = state.objectFetcher
      if (fetcher.isFetching) {
        if (state.lcdControl.objEnable) fetcher.step.tick(state)
        else fetcher.cancel()
        false
      } else if (objectFetchReady(state)) {
        fetcher.startFetch(fetcher.scanlineObjectIndex)
        fetcher.scanlineObjectIndex += 1
        state.backgroundFetcher.restartForObjectFetch()
        false
      } else {
        state.backgroundFetcher.startWindow(state)
        state.backgroundFetcher.step.tick(state)
        true
      }
    }

    /**
     * Whether the next object is due on this scanline. Objects come due left to right,
     * so the buffer having been put into draw order makes the first one not yet passed
     * the rule for the rest.
     *
     * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#sprite-fetching]]
     */
    private def objectFetchReady(state: Ppu.State): Boolean = {
      val fetcher = state.objectFetcher
      state.lcdControl.objEnable &&
        GameboyObject.isObjectAtIndexReady(
          state.scanlineObjects,
          fetcher.scanlineObjectIndex,
          state.pixelMixer.shiftPosition
        )
    }
  }
}
