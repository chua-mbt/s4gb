package org.akaii.s4gb.jvmshell

import org.akaii.s4gb.emulator.components.Joypad.Button
import java.awt.{BorderLayout, Frame, GridLayout}
import javax.swing._
import javax.swing.border.EmptyBorder

/** Lists the Game Boy buttons with their current key. Clicking one arms a rebind. */
final class BindingsDialog(owner: Frame, input: Input)
  extends JDialog(owner, "Key bindings", java.awt.Dialog.ModalityType.APPLICATION_MODAL) {

  private val keys: Map[Button, JButton] =
    Button.values.map(button => button -> new JButton()).toMap

  input.onBindingChanged = () => refresh()

  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  addWindowListener(new java.awt.event.WindowAdapter {
    override def windowClosed(e: java.awt.event.WindowEvent): Unit = input.cancelCapture()
  })

  private val hint = new JLabel("Click a key, then press the key you want. Esc cancels.")
  hint.setBorder(new EmptyBorder(8, 8, 8, 8))

  private val grid = new JPanel(new GridLayout(Button.values.length, 2, 6, 6))
  grid.setBorder(new EmptyBorder(8, 8, 8, 8))
  Button.values.foreach { button =>
    grid.add(new JLabel(buttonLabel(button), SwingConstants.RIGHT))
    val key = keys(button)
    key.addActionListener(_ => {
      input.beginCapture(button)
      refresh()
    })
    grid.add(key)
  }

  setLayout(new BorderLayout())
  add(hint, BorderLayout.NORTH)
  add(grid, BorderLayout.CENTER)
  pack()
  setLocationRelativeTo(owner)

  refresh()

  def showDialog(): Unit = {
    setVisible(true)
    input.onBindingChanged = () => ()
  }

  private def buttonLabel(button: Button): String = button match {
    case Button.A       => "A"
    case Button.B       => "B"
    case Button.Start   => "Start"
    case Button.Select  => "Select"
    case other          => other.toString
  }

  private def refresh(): Unit = keys.foreach { case (button, key) => key.setText(input.label(button)) }
}
