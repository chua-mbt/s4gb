package org.akaii.s4gb.jvmshell

import org.akaii.s4gb.emulator.components.Joypad.{Button, ButtonState}
import java.awt.event.KeyEvent
import java.awt.{KeyEventDispatcher, KeyboardFocusManager, Window}

/**
 * Routes keyboard events to the joypad. Installed on the focus manager rather than on a
 * component, so the panel does not have to hold focus. Events are queued and applied on
 * the emulation thread, so a key press never mutates joypad state from the EDT.
 *
 * While a capture is armed every key press goes to the rebind instead of the game, and
 * keys are only routed to the game when the main window is the active one, so file
 * choosers keep their arrow keys.
 */
final class Input(loop: EmulatorLoop, mainWindow: Window) extends KeyEventDispatcher {

  private var bindings = KeyBindings.Default
  private var capturing: Option[Button] = None

  /** Fires whenever a capture starts or resolves, so the bindings dialog can repaint. */
  var onBindingChanged: () => Unit = () => ()

  KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher(this)

  def captureTarget: Option[Button] = capturing

  def label(button: Button): String =
    if (capturing.contains(button)) "press a key..."
    else bindings.keyFor(button).map(Input.keyName).getOrElse("unbound")

  def beginCapture(button: Button): Unit = {
    capturing = Some(button)
    onBindingChanged()
  }

  def cancelCapture(): Unit =
    if (capturing.isDefined) {
      capturing = None
      onBindingChanged()
    }

  override def dispatchKeyEvent(event: KeyEvent): Boolean = {
    val code = event.getKeyCode
    val pressed = event.getID == KeyEvent.KEY_PRESSED
    val released = event.getID == KeyEvent.KEY_RELEASED

    capturing match {
      case Some(_) if Input.Modifiers.contains(code) => true

      case Some(target) if pressed =>
        capturing = None
        if (code != KeyEvent.VK_ESCAPE && code != KeyEvent.VK_UNDEFINED)
          bindings = bindings.bind(code, target)
        onBindingChanged()
        true

      case Some(_) if released =>
        true

      case _ if (pressed || released) && mainIsActive =>
        bindings.buttonFor(code).fold(false) { button =>
          loop.setButton(button, if (pressed) ButtonState.Pressed else ButtonState.Released)
          true
        }

      case _ => false
    }
  }

  private def mainIsActive: Boolean =
    KeyboardFocusManager.getCurrentKeyboardFocusManager.getActiveWindow eq mainWindow
}

object Input {

  private val Modifiers: Set[Int] =
    Set(KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH, KeyEvent.VK_META)

  def keyName(code: Int): String = {
    val text = KeyEvent.getKeyText(code)
    if (text.isEmpty) text else text.head.toUpper.toString + text.tail
  }
}
