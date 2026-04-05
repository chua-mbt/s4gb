package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

/**
 * Gameboy terminology for sprites. Max of 40 can be displayed, 10 max on any scanline.
 *
 * @see [[https://gbdev.io/pandocs/Graphics.html#objects]]
 */
case class GameboyObject(
  var y: UByte = 0.toUByte,
  var x: UByte = 0.toUByte,
  var tileIndex: UByte = 0.toUByte,
  var attributes: UByte = 0.toUByte,
  var used: Boolean = false
) {
  def notInUse: Boolean = !used

  def reset(): Unit = used = false

  def set(y: UByte, x: UByte, tileIndex: UByte, attributes: UByte): Unit = {
    this.y = y
    this.x = x
    this.tileIndex = tileIndex
    this.attributes = attributes
    used = true
  }
}

object GameboyObject {
  def empty: GameboyObject = GameboyObject()
}
