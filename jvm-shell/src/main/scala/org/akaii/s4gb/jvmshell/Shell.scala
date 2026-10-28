package org.akaii.s4gb.jvmshell

import org.akaii.s4gb.emulator.Emulator

import javax.swing.*
import java.awt.BorderLayout
import java.awt.event.KeyEvent
import java.nio.file.{Path, Paths}

object Shell {

  private val Scale = 4

  def main(args: Array[String]): Unit =
    SwingUtilities.invokeLater(() => build(args))

  private def build(args: Array[String]): Unit = {
    val screen = new ScreenPanel(Scale)
    val frame = new JFrame()
    var romName = Option.empty[String]

    def updateTitle(frameRate: Double): Unit =
      SwingUtilities.invokeLater(() => frame.setTitle(title(romName, frameRate)))

    val loop = new EmulatorLoop(screen, updateTitle)

    def load(path: Path): Unit =
      RomLoader.read(path) match {
        case Right(rom) =>
          loop.load(rom.bytes)
          romName = Some(rom.name)
          updateTitle(0.0)
        case Left(RomError.ReadFailed(failedPath, error)) =>
          showLoadError(frame, failedPath, error)
      }

    val chooser = new JFileChooser()
    val openItem = new JMenuItem("Open ROM...")
    openItem.setMnemonic(KeyEvent.VK_O)
    openItem.addActionListener(_ =>
      if (chooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION)
        load(chooser.getSelectedFile.toPath)
    )

    val input = new Input(loop, frame)
    val bindingsItem = new JMenuItem("Key bindings...")
    bindingsItem.addActionListener(_ => new BindingsDialog(frame, input).showDialog())

    frame.setJMenuBar(buildMenuBar(openItem, bindingsItem))
    args.headOption.foreach(path => load(Paths.get(path)))

    frame.setLayout(new BorderLayout())
    frame.add(screen, BorderLayout.CENTER)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    updateTitle(0.0)

    loop.start()
  }

  private def buildMenuBar(openItem: JMenuItem, bindingsItem: JMenuItem): JMenuBar = {
    val fileMenu = new JMenu("File")
    fileMenu.add(openItem)
    val inputMenu = new JMenu("Input")
    inputMenu.add(bindingsItem)
    val bar = new JMenuBar()
    bar.add(fileMenu)
    bar.add(inputMenu)
    bar
  }

  private def showLoadError(frame: JFrame, path: Path, error: Throwable): Unit = {
    System.err.println(s"Could not load ROM $path: $error")
    JOptionPane.showMessageDialog(frame, error.getMessage, "Could not load ROM", JOptionPane.ERROR_MESSAGE)
  }

  private def title(romName: Option[String], frameRate: Double): String =
    romName match {
      case Some(name) => f"s4gb | $name | $frameRate%.1f fps"
      case None       => "s4gb | no ROM loaded"
    }
}
