package org.akaii.s4gb.jvmshell

import org.akaii.s4gb.emulator.components.ppu.{PixelEmitter, Ppu}
import spire.math.UByte

import java.awt.image.BufferedImage
import java.awt.{Dimension, Graphics, Graphics2D, RenderingHints}
import javax.swing.JPanel

/**
 * The window body. Pixels arrive on the emulation thread through `emit` and are published
 * to the EDT as a whole frame by `present`, so a repaint never sees a half drawn screen.
 */
final class ScreenPanel(val scale: Int = 4) extends JPanel with PixelEmitter {

  private val columns = Ppu.VISIBLE_WIDTH
  private val rows = Ppu.VISIBLE_HEIGHT

  private var back: Array[Array[UByte]] = blank()
  @volatile private var front: Array[Array[UByte]] = blank()

  private val image = new BufferedImage(columns, rows, BufferedImage.TYPE_INT_RGB)
  private val pixels = Array.fill(columns * rows)(0)

  setPreferredSize(Dimension(columns * scale, rows * scale))

  override def emit(x: Int, y: Int, color: UByte): Unit =
    if (x >= 0 && x < columns && y >= 0 && y < rows) back(y)(x) = color

  /** Publishes the back buffer and asks for a repaint. Emulation thread only. */
  def present(): Unit = {
    front = back.map(_.clone())
    repaint()
  }

  /** Drops both buffers, so a freshly loaded ROM does not open on the previous picture. */
  def clear(): Unit = {
    back = blank()
    front = back
    repaint()
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    val frame = front
    var i = 0
    while (i < pixels.length) {
      pixels(i) = ScreenPanel.Shades(frame(i / columns)(i % columns).toInt)
      i += 1
    }
    image.setRGB(0, 0, columns, rows, pixels, 0, columns)

    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
    g2.drawImage(image, 0, 0, columns * scale, rows * scale, null)
  }

  private def blank(): Array[Array[UByte]] = Array.fill(rows, columns)(UByte(0))
}

object ScreenPanel {

  /** Shade index to packed RGB, lightest first. */
  private val Shades: Array[Int] = Array(0xFFFFFF, 0xAAAAAA, 0x555555, 0x000000)
}
