package org.akaii.s4gb.jvmshell

import org.akaii.s4gb.emulator.components.Joypad.Button
import java.awt.event.KeyEvent

final case class KeyBindings(byKey: Map[Int, Button]) {

  def buttonFor(code: Int): Option[Button] = byKey.get(code)

  def keyFor(button: Button): Option[Int] =
    byKey.collectFirst { case (code, bound) if bound == button => code }

  def bind(code: Int, button: Button): KeyBindings =
    KeyBindings(byKey.filterNot { case (key, bound) => key == code || bound == button } + (code -> button))
}

object KeyBindings {

  val Default: KeyBindings = KeyBindings(Map(
    KeyEvent.VK_LEFT -> Button.Left,
    KeyEvent.VK_RIGHT -> Button.Right,
    KeyEvent.VK_UP -> Button.Up,
    KeyEvent.VK_DOWN -> Button.Down,
    KeyEvent.VK_Z -> Button.A,
    KeyEvent.VK_X -> Button.B,
    KeyEvent.VK_ENTER -> Button.Start,
    KeyEvent.VK_BACK_SPACE -> Button.Select
  ))
}
