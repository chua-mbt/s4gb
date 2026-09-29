package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Pixel Mixing Logic
 *
 * @see [[https://gbdev.io/pandocs/pixel_fifo.html]]
 * @see [[https://gbdev.io/pandocs/Rendering.html#pixel-rendering]]
 */
case class PixelMixer(
  backgroundPixel: BackgroundPixel = BackgroundPixel.empty,
  objectPixel: ObjectPixel = ObjectPixel.empty
) {

  /**
   * Only pop when there's a pixel in both queues.
   */
  def tick(state: Ppu.State, emitter: PixelEmitter): Unit = {
    // Stop processing if we have reached the end of the scanline
    if (state.scanlineDot.current >= 160) return

    // Both queues are synchronized
    if (!state.backgroundFifo.isEmpty && !state.objectFifo.isEmpty) {
      state.backgroundFifo.pop(backgroundPixel)
      state.objectFifo.pop(objectPixel)
      mixAndEmit(state, emitter)
    }
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

    emitter.emit(state.scanlineDot.current, state.ly.toInt, color)
  }
}
