package org.akaii.s4gb.jsshell

import org.akaii.s4gb.emulator.{Config, Emulator}
import org.akaii.s4gb.emulator.components.Joypad.{Button, ButtonState}
import org.akaii.s4gb.emulator.components.Rom
import org.akaii.s4gb.emulator.components.ppu.{PixelEmitter, Ppu}
import org.scalajs.dom
import org.scalajs.dom.html
import spire.math.UByte

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

object Shell {

  private val Width = Ppu.VISIBLE_WIDTH
  private val Height = Ppu.VISIBLE_HEIGHT
  private val CyclesPerFrame = 70224 / 4
  private val Shades = Array(0xFFFFFF, 0xAAAAAA, 0x555555, 0x000000)

  def main(args: Array[String]): Unit = {
    val body = dom.document.body

    val stats = dom.document.createElement("div")
    stats.textContent = "fps: --"
    body.appendChild(stats)

    val fileInput = dom.document.createElement("input").asInstanceOf[html.Input]
    fileInput.`type` = "file"
    body.appendChild(fileInput)

    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.width = Width
    canvas.height = Height
    canvas.style.width = (Width * 4).toString + "px"
    canvas.style.height = (Height * 4).toString + "px"
    body.appendChild(canvas)

    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val imageData = ctx.createImageData(Width, Height)

    val back = new Array[Int](Width * Height)
    val front = new Array[Int](Width * Height)

    val emitter: PixelEmitter = (x, y, color) => {
      if (x >= 0 && x < Width && y >= 0 && y < Height) {
        back(y * Width + x) = color.toInt
      }
    }

    var machine: Option[Emulator] = None

    def clear(): Unit = {
      var i = 0
      while (i < back.length) { back(i) = 0; i += 1 }
      present()
    }

    def present(): Unit = {
      System.arraycopy(back, 0, front, 0, back.length)
      var i = 0
      while (i < front.length) {
        val shade = Shades(front(i) & 3)
        val idx = i * 4
        imageData.data(idx) = ((shade >> 16) & 0xFF)
        imageData.data(idx + 1) = ((shade >> 8) & 0xFF)
        imageData.data(idx + 2) = (shade & 0xFF)
        imageData.data(idx + 3) = 255
        i += 1
      }
      ctx.putImageData(imageData, 0, 0)
    }

    def loadRom(bytes: Array[Byte]): Unit = {
      clear()
      val rom = Rom(bytes.map(b => UByte(b & 0xFF)))
      val m = Emulator(rom, emitter, Config())
      m.initialize()
      machine = Some(m)
    }

    fileInput.addEventListener("change", (_: dom.Event) => {
      val files = fileInput.files
      if (files.length > 0) {
        val reader = new dom.FileReader()
        reader.onload = (_: dom.Event) => {
          val buffer = reader.result.asInstanceOf[js.typedarray.ArrayBuffer]
          val view = new Uint8Array(buffer)
          val bytes = new Array[Byte](view.length)
          var i = 0
          while (i < view.length) {
            bytes(i) = view(i).toByte
            i += 1
          }
          loadRom(bytes)
        }
        reader.readAsArrayBuffer(files(0))
      }
    })

    val keyMap: Map[Int, Button] = Map(
      37 -> Button.Left,
      38 -> Button.Up,
      39 -> Button.Right,
      40 -> Button.Down,
      90 -> Button.A,
      88 -> Button.B,
      13 -> Button.Start,
      8 -> Button.Select
    )

    dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      keyMap.get(e.keyCode).foreach { button =>
        machine.foreach(_.joypad.setButtonState(button, ButtonState.Pressed))
        e.preventDefault()
      }
    })

    dom.window.addEventListener("keyup", (e: dom.KeyboardEvent) => {
      keyMap.get(e.keyCode).foreach { button =>
        machine.foreach(_.joypad.setButtonState(button, ButtonState.Released))
        e.preventDefault()
      }
    })

    var windowStart = dom.window.performance.now()
    var windowFrames = 0

    def frame(now: Double): Unit = {
      machine.foreach { m =>
        var ticks = 0
        var ly = m.ppu(Ppu.Address.LY).toInt
        var presented = false
        while (ticks < CyclesPerFrame) {
          m.tick()
          val next = m.ppu(Ppu.Address.LY).toInt
          if (!presented && ly > 0 && next == 0) {
            present()
            presented = true
          }
          ly = next
          ticks += 1
        }
        if (!presented) present()
      }

      windowFrames += 1
      val elapsed = now - windowStart
      if (elapsed >= 1000.0) {
        val rate = windowFrames * 1000.0 / elapsed
        stats.textContent = f"fps: $rate%.2f"
        windowStart = now
        windowFrames = 0
      }

      dom.window.requestAnimationFrame((t: Double) => frame(t))
    }

    dom.window.requestAnimationFrame((t: Double) => frame(t))
  }
}