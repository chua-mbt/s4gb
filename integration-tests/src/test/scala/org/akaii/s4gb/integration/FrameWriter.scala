package org.akaii.s4gb.integration

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

/**
 * Writes emulator frames out as PNGs. dmg-acid2 compares byte for byte against
 * `img/reference-dmg.png`, so the ramp has to be exactly `$00`, `$55`, `$AA`, `$FF`.
 */
object FrameWriter {

  /** Shade index to 8 bit greyscale. Index 0 is the lightest shade, hence the inversion. */
  val Greyscale: Seq[Int] = Seq(0xFF, 0xAA, 0x55, 0x00)

  /** Nearest neighbour magnification, so a 160x144 frame is legible on screen. */
  def write(frame: Frame, file: File, scale: Int = 3): File = {
    ImageIO.write(render(frame, scale), "png", file)
    file
  }

  def render(frame: Frame, scale: Int = 1): BufferedImage = {
    val image = new BufferedImage(frame.width * scale, frame.height * scale, BufferedImage.TYPE_INT_RGB)
    val out = image.getRaster
    for {
      y <- 0 until frame.height
      x <- 0 until frame.width
      g = Greyscale(frame.shade(x, y))
      dy <- 0 until scale
      dx <- 0 until scale
    } out.setPixel(x * scale + dx, y * scale + dy, Array(g, g, g))
    image
  }

  /** The reference is 2bpp, which ImageIO hands back with one bit per sample, so rescale. */
  def readGreyscale(file: File): Option[Array[Array[Int]]] =
    Option(ImageIO.read(file)).map { img =>
      val raster = img.getRaster
      val sample = Array(0)
      Array.tabulate(img.getHeight, img.getWidth) { (y, x) =>
        val raw = raster.getPixel(x, y, sample)(0)
        val max = (1 << raster.getSampleModel.getSampleSize(0)) - 1
        raw * 255 / max
      }
    }

  /** Mismatches in red, matches dimmed. Returns the mismatch count. */
  def writeDiff(frame: Frame, reference: Array[Array[Int]], file: File, scale: Int = 3): Int = {
    val mismatches = Array.tabulate(frame.height, frame.width) { (y, x) =>
      !sameShade(frame.shade(x, y), reference(y)(x))
    }
    val count = mismatches.flatten.count(identity)

    val image = new BufferedImage(frame.width * scale, frame.height * scale, BufferedImage.TYPE_INT_RGB)
    val out = image.getRaster
    for {
      y <- 0 until frame.height
      x <- 0 until frame.width
      (r, g, b) = if (mismatches(y)(x)) (0xFF, 0x00, 0x00) else (0x30, 0x30, 0x30)
      dy <- 0 until scale
      dx <- 0 until scale
    } {
      out.setSample(x * scale + dx, y * scale + dy, 0, r)
      out.setSample(x * scale + dx, y * scale + dy, 1, g)
      out.setSample(x * scale + dx, y * scale + dy, 2, b)
    }
    ImageIO.write(image, "png", file)
    count
  }

  private def sameShade(emitted: Int, referenceSample: Int): Boolean =
    referenceSample == Greyscale(emitted)
}
