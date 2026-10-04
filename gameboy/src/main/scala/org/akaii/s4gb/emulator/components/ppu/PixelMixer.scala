package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Pixel Mixing Logic. Controls pixel shifting, so it also owns the tracking for how far
 * along the line to shift.
 *
 * @param backgroundPixel Reused holder the background FIFO pops into.
 * @param objectPixel Reused holder the object FIFO pops into.
 * @param shiftPosition Shifter position, counting the pixels the fine scroll drops.
 * @param fineScroll Fine scroll offset locked from SCX at the scanline start, `SCX % 8`.
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html#pixel-rendering]]
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html#pushing-pixels-to-the-lcd]]
 * @see [[https://gbdev.io/pandocs/Rendering.html#obj-penalty-algorithm]]
 * @see [[https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#scx-at-a-sub-tile-layer]]
 */
case class PixelMixer(
  backgroundPixel: BackgroundPixel = BackgroundPixel.empty,
  objectPixel: ObjectPixel = ObjectPixel.empty,
  var shiftPosition: Int = 0,
  private var fineScroll: Int = 0
) {

  /** Pixels emitted this scanline, also the output x. The fine scroll drops the first
   * `fineScroll` positions, so this lags [[shiftPosition]] by that much until they meet.
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#pushing-pixels-to-the-lcd]] */
  def renderedPixels: Int = math.max(0, shiftPosition - fineScroll)

  /**
   * Resets the counters for a new scanline, taking the fine scroll from SCX.
   *
   * @see [[https://gbdev.io/pandocs/Scrolling.html#mid-frame-behavior]]
   */
  def beginScanline(state: Ppu.State): Unit = {
    shiftPosition = 0
    fineScroll = state.registers(Ppu.Address.SCX).toInt % Tile.SIZE
  }

  /**
   * The background FIFO is the gate: an empty object FIFO contributes a
   * transparent pixel rather than stalling the line.
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#pixel-rendering]]
   */
  def tick(state: Ppu.State, emitter: PixelEmitter): Unit = {
    if (renderedPixels >= Ppu.VISIBLE_WIDTH) return

    if (!state.backgroundFifo.pop(backgroundPixel)) return
    if (!state.objectFifo.pop(objectPixel)) objectPixel.colorIndex = ObjectPixel.transparentColor

    if (shiftPosition < fineScroll) {
      shiftPosition += 1
      return
    }

    mixAndEmit(state, emitter)
    shiftPosition += 1
  }

  /**
   * Determine whether background or object takes priority for this pixel and emit
   *
   * @see [[https://gbdev.io/pandocs/pixel_fifo.html#pixel-rendering]]
   * @see [[https://gbdev.io/pandocs/LCDC.html#lcdc0--bg-and-window-enablepriority]]
   * @see [[https://gbdev.io/pandocs/LCDC.html#lcdc1--obj-enable]]
   * @see [[https://gbdev.io/pandocs/OAM.html#drawing-priority]]
   */
  private def mixAndEmit(
    state: Ppu.State,
    emitter: PixelEmitter
  ): Unit = {
    val lcdc = state.lcdControl

    val bgIsLightestColor = backgroundPixel.colorIndex == BackgroundPixel.lightestColor
    val objectHasPriority = !lcdc.bgEnable || bgIsLightestColor || objectPixel.isOverBackground
    val useObject = lcdc.objEnable && objectPixel.isOpaque && objectHasPriority

    val paletteAddr =
      if (!useObject) Ppu.Address.BGP
      else if (objectPixel.usePalette0) Ppu.Address.OBP0
      else Ppu.Address.OBP1

    val palette = state.registers(paletteAddr)

    val colorIndex =
      if (useObject) objectPixel.colorIndex
      else if (lcdc.bgEnable) backgroundPixel.colorIndex
      else BackgroundPixel.lightestColor

    val color = Pixel.resolvePixelColor(colorIndex, palette)

    emitter.emit(renderedPixels, state.ly.toInt, color)
  }
}
