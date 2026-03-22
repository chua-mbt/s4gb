package org.akaii.s4gb.emulator.components

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

import scala.collection.mutable

/**
 * Represents the GameBoy's joypad input register (P1/JOYP)
 *
 * @see [[https://gbdev.io/pandocs/Joypad_Input.html]]
 */
case class Joypad(interrupts: Interrupts) extends MemoryMap {

  import Joypad.*
  import Joypad.Address.*
  import Joypad.Button.*
  import Joypad.ButtonState.*
  import Joypad.SelectorState.*

  private[components] val buttons: mutable.Map[Button, ButtonState] =
    Button.values.map(button => button -> Released).to(mutable.Map)

  /**
   * Select d-pad: If this bit is 0, then directional keys can be read from the lower nibble.
   */
  private[components] var dPadSelector: SelectorState = Unselected

  /**
   * Select buttons: If this bit is 0, then buttons (SsBA) can be read from the lower nibble.
   */
  private[components] var buttonSelector: SelectorState = Unselected

  /**
   * Updates the state of a button. If the button transitions from released to pressed, an interrupt is requested.
   *
   * @see [[https://gbdev.io/pandocs/Interrupt_Sources.html#int-60--joypad-interrupt]]
   */
  def setButtonState(button: Button, state: ButtonState): Unit = {
    if(buttons(button) == Released && state == Pressed) interrupts.request(Interrupts.Source.Joypad)
    buttons(button) = state
  }

  /**
   * Lower nibble represents button states and depends on state of selectors.
   *
   * @see [[https://mgba-emu.github.io/gbdoc/#mmio-p1]]
   */
  override def apply(address: UShort): UByte = {
    if (address != Address.JOYPAD)
      throw new IllegalArgumentException(s"Invalid address: $address")

    val selectorBits: UByte = dPadSelector.shift(ButtonIndex.Select_DPad) | buttonSelector.shift(ButtonIndex.SelectButton)

    lazy val dPadStates: Seq[UByte] = Seq(Button.Right, Button.Left, Button.Up, Button.Down).map(shiftButton)
    lazy val buttonStates: Seq[UByte] = Seq(Button.A, Button.B, Button.Select, Button.Start).map(shiftButton)
    lazy val andDPadAndButtons: Seq[UByte] = dPadStates.zip(buttonStates).map { case (d, b) => d & b }

    val buttonBits: UByte = (dPadSelector, buttonSelector) match {
      case (Unselected, Unselected) => Masks.BUTTON_STATE
      case (Unselected, Selected) => consolidateStates(buttonStates)
      case (Selected, Unselected) => consolidateStates(dPadStates)
      case (Selected, Selected) => consolidateStates(andDPadAndButtons)
    }

    Masks.IGNORED | selectorBits | buttonBits
  }

  /**
   * The lower nibble is Read-only.
   */
  override def write(address: UShort, value: UByte): Unit = {
    if (address != Address.JOYPAD)
      throw new IllegalArgumentException(s"Invalid address: $address")

    dPadSelector = SelectorState.values((value >> ButtonIndex.Select_DPad.bit).toInt & 0x1)
    buttonSelector = SelectorState.values((value >> ButtonIndex.SelectButton.bit).toInt & 0x1)
  }

  override def toString: String = {
    val allButtonsStr = buttons.toSeq
      .sortBy(_._1.index.bit)
      .map { case (btn, state) => s"${btn.toString}=${state}" }
      .mkString(", ")

    val byteHex = apply(Address.JOYPAD).toInt.toHexString.toUpperCase

    s"Joypad(DPadSelector=$dPadSelector, ButtonSelector=$buttonSelector, " +
      s"AllButtons=[$allButtonsStr], JOYP=0x$byteHex)"
  }

  private def shiftButton(button: Button): UByte =
    buttons(button).value << button.index.bit

  private def consolidateStates(states: Seq[UByte]): UByte =
    states.foldLeft(0.toUByte)(_ | _)
}

object Joypad {
  object Address {
    /**
     * Joypad (P1/JOYP)
     *
     * @see [[https://gbdev.io/pandocs/Joypad_Input.html#ff00--p1joyp-joypad]]
     */
    val JOYPAD: UShort = UShort(0xFF00)
  }

  object Masks {
    val IGNORED: UByte = 0xC0.toUByte // Bits 6-7 are always read as 1
    val SELECTORS: UByte = 0x30.toUByte // Bits 4-5 = 0 to select group
    val BUTTON_STATE: UByte = 0x0F.toUByte // Bits 0-3 represent button states (active low)
    val ALL: UByte = 0xFF.toUByte
  }

  /**
   * Position corresponding to a bit in the JOYP register.
   *
   * @see [[https://gbdev.io/pandocs/Joypad_Input.html#ff00--p1joyp-joypad]]
   */
  enum ButtonIndex(val bit: Int) {
    case A_Right extends ButtonIndex(0)
    case B_Left extends ButtonIndex(1)
    case Select_Up extends ButtonIndex(2)
    case Start_Down extends ButtonIndex(3)
    case Select_DPad extends ButtonIndex(4)
    case SelectButton extends ButtonIndex(5)
  }

  enum Button(val index: ButtonIndex, val selectorIndex: ButtonIndex) {
    case A extends Button(ButtonIndex.A_Right, ButtonIndex.SelectButton)
    case B extends Button(ButtonIndex.B_Left, ButtonIndex.SelectButton)
    case Select extends Button(ButtonIndex.Select_Up, ButtonIndex.SelectButton)
    case Start extends Button(ButtonIndex.Start_Down, ButtonIndex.SelectButton)
    case Right extends Button(ButtonIndex.A_Right, ButtonIndex.Select_DPad)
    case Left extends Button(ButtonIndex.B_Left, ButtonIndex.Select_DPad)
    case Up extends Button(ButtonIndex.Select_Up, ButtonIndex.Select_DPad)
    case Down extends Button(ButtonIndex.Start_Down, ButtonIndex.Select_DPad)
  }

  /**
   * Semantics of a button's state in the JOYP register.
   *
   * @see [[https://gbdev.io/pandocs/Joypad_Input.html#ff00--p1joyp-joypad]]
   */
  enum ButtonState(val value: UByte) {
    case Pressed extends ButtonState(0.toUByte)
    case Released extends ButtonState(1.toUByte)
  }

  enum SelectorState(val value: UByte) {
    case Selected extends SelectorState(0.toUByte)
    case Unselected extends SelectorState(1.toUByte)

    def shift(buttonIndex: ButtonIndex): UByte =
      (value << buttonIndex.bit) & Masks.ALL
  }
}
