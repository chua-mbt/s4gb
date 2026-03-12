package org.akaii.s4gb.emulator.io

import munit.FunSuite
import org.akaii.s4gb.emulator.Interrupts
import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.io.Joypad.ButtonIndex
import spire.math.UByte

class JoypadTests extends FunSuite {

  import Joypad.ButtonIndex.*
  import Joypad.ButtonState.*
  import Joypad.SelectorState.*

  test("initial state") {
    val joypad = Joypad(Interrupts())
    assertEquals(joypad(Joypad.Address.JOYPAD), UByte(0xFF))
    assertEquals(joypad.dPadSelector, Unselected)
    assertEquals(joypad.buttonSelector, Unselected)
    assert(joypad.buttons.values.forall(_ == Released))
    assert(joypad.toString.contains("DPadSelector=Unselected"))
    assert(joypad.toString.contains("ButtonSelector=Unselected"))
    assert(!joypad.toString.contains("Pressed"))
    assert(joypad.toString.contains("JOYP=0xFF"))
  }

  test("writes all selector combinations") {
    val combinations = Seq(
      (0x30.toUByte, Unselected, Unselected, 0xFF.toUByte),
      (0x20.toUByte, Selected, Unselected, 0xEF.toUByte),
      (0x10.toUByte, Unselected, Selected, 0xDF.toUByte),
      (0x00.toUByte, Selected, Selected, 0xCF.toUByte)
    )

    combinations.foreach { case (writeVal, expectedDPad, expectedButton, expectedJoyp) =>
      val joypad = Joypad(Interrupts())
      joypad.write(Joypad.Address.JOYPAD, writeVal)
      assertEquals(joypad.dPadSelector, expectedDPad)
      assertEquals(joypad.buttonSelector, expectedButton)
      assert(joypad.buttons.values.forall(_ == Released))
      assertEquals(joypad(Joypad.Address.JOYPAD), expectedJoyp)
      assert(joypad.toString.contains(s"DPadSelector=$expectedDPad"))
      assert(joypad.toString.contains(s"ButtonSelector=$expectedButton"))
      assert(!joypad.toString.contains("Pressed"))
      assert(joypad.toString.contains(s"JOYP=0x${expectedJoyp.toInt.toHexString.toUpperCase}"))
    }
  }

  test("each button can be pressed and released") {
    Joypad.Button.values.foreach { button =>
      val joypad = Joypad(Interrupts())

      // Set selectors so the button’s group is active
      val dPadSel = if (button.selectorIndex == Select_DPad) 0.toUByte else 1.toUByte
      val btnSel = if (button.selectorIndex == SelectButton) 0.toUByte else 1.toUByte
      val selectorValue = (btnSel << SelectButton.bit) | (dPadSel << Select_DPad.bit)
      joypad.write(Joypad.Address.JOYPAD, selectorValue)

      // Press the button
      joypad.setButtonState(button, Joypad.ButtonState.Pressed)
      assertEquals(joypad.buttons(button), Joypad.ButtonState.Pressed, clue = button)
      assert(joypad.buttons.filterNot(_._1 == button).values.forall(_ == Joypad.ButtonState.Released), clue = button)
      val pressed = joypad(Joypad.Address.JOYPAD)
      assertEquals((pressed >> button.index.bit) & 0x1.toUByte, 0.toUByte, clue = button)
      assert(joypad.toString.contains(s"$button=Pressed"), clue = button)

      // Release the button
      joypad.setButtonState(button, Joypad.ButtonState.Released)
      assertEquals(joypad.buttons(button), Joypad.ButtonState.Released, clue = button)
      assert(joypad.buttons.filterNot(_._1 == button).values.forall(_ == Joypad.ButtonState.Released), clue = button)
      val released = joypad(Joypad.Address.JOYPAD)
      assertEquals((released >> button.index.bit) & 0x1.toUByte, 1.toUByte, clue = button)
      assert(joypad.toString.contains(s"$button=Released"), clue = button)
    }
  }

  test("both selectors unselected ignores button states") {
    val joypad = Joypad(Interrupts())
    Joypad.Button.values.foreach(b => joypad.setButtonState(b, Joypad.ButtonState.Pressed))

    val selectorValue = 0x30.toUByte
    joypad.write(Joypad.Address.JOYPAD, selectorValue)

    assertEquals(joypad.buttonSelector, Unselected)
    assertEquals(joypad.dPadSelector, Unselected)

    val byteValue = joypad(Joypad.Address.JOYPAD)
    val lowerNibble = byteValue & 0x0F.toUByte
    assertEquals(lowerNibble, 0x0F.toUByte)
  }

  test("both selectors combine button groups with AND") {
    val joypad = Joypad(Interrupts())

    joypad.write(Joypad.Address.JOYPAD, 0x00.toUByte) // both selected

    joypad.setButtonState(Joypad.Button.Right, Joypad.ButtonState.Pressed)
    joypad.setButtonState(Joypad.Button.A, Joypad.ButtonState.Released)

    val value = joypad(Joypad.Address.JOYPAD)

    assertEquals((value >> ButtonIndex.A_Right.bit) & 0x1.toUByte, 0.toUByte)
  }

  test("multiple buttons under the same selector pressed") {
    val joypad = Joypad(Interrupts())

    joypad.setButtonState(Joypad.Button.Up, Joypad.ButtonState.Pressed)
    joypad.setButtonState(Joypad.Button.Left, Joypad.ButtonState.Pressed)

    joypad.write(Joypad.Address.JOYPAD, 0x10.toUByte)
    val buttonValue = joypad(Joypad.Address.JOYPAD)
    assertEquals((buttonValue >> ButtonIndex.Select_Up.bit) & 0x1.toUByte, 1.toUByte)
    assertEquals((buttonValue >> ButtonIndex.B_Left.bit) & 0x1.toUByte, 1.toUByte)

    joypad.write(Joypad.Address.JOYPAD, 0x20.toUByte)
    val dPadValue = joypad(Joypad.Address.JOYPAD)
    assertEquals((dPadValue >> ButtonIndex.Select_Up.bit) & 0x1.toUByte, 0.toUByte)
    assertEquals((dPadValue >> ButtonIndex.B_Left.bit) & 0x1.toUByte, 0.toUByte)
  }

  test("pressing buttons triggers Joypad interrupt") {
    Joypad.Button.values.foreach { button =>
      val interrupts = Interrupts()
      val joypad = Joypad(interrupts)

      val dPadSel = if (button.selectorIndex == Select_DPad) 0.toUByte else 1.toUByte
      val btnSel = if (button.selectorIndex == SelectButton) 0.toUByte else 1.toUByte
      val selectorValue = (btnSel << SelectButton.bit) | (dPadSel << Select_DPad.bit)
      joypad.write(Joypad.Address.JOYPAD, selectorValue)

      assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG) & (1 << 4).toUByte, 0.toUByte, clue = button)

      // RELEASE -> PRESS
      joypad.setButtonState(button, Pressed)
      assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG) & (1 << 4).toUByte, (1 << 4).toUByte, clue = button)
      interrupts.clear(Interrupts.Source.Joypad)

      // PRESS -> PRESS
      joypad.setButtonState(button, Pressed)
      assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG), Interrupts.Masks.IGNORED, clue = button)
      interrupts.clear(Interrupts.Source.Joypad)

      // PRESS -> RELEASE
      joypad.setButtonState(button, Released)
      assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG), Interrupts.Masks.IGNORED, clue = button)
      interrupts.clear(Interrupts.Source.Joypad)

      // RELEASE -> RELEASE
      joypad.setButtonState(button, Released)
      assertEquals(interrupts(Interrupts.Address.INTERRUPT_FLAG), Interrupts.Masks.IGNORED, clue = button)
      interrupts.clear(Interrupts.Source.Joypad)
    }
  }
}
