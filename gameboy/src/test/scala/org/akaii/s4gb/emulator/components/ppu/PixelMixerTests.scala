package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.emulator.components.ppu.Ppu.State
import spire.math.UByte

class PixelMixerTests extends FunSuite {
  import PixelMixerTests.*

  test("object wins over a non-lightest background when it has priority") {
    assertEquals(emit(MixerCase()), Some(UByte(2)))
  }

  test("background wins when the object is behind it") {
    assertEquals(emit(MixerCase(objOverBackground = false)), Some(UByte(1)))
  }

  test("object wins over the lightest background even when it is behind it") {
    assertEquals(emit(MixerCase(bgColor = 0, objOverBackground = false)), Some(UByte(2)))
  }

  test("background wins when objects are disabled") {
    assertEquals(emit(MixerCase(objEnabled = false)), Some(UByte(1)))
  }

  test("object wins when the background is disabled even when it is behind it") {
    assertEquals(emit(MixerCase(bgEnabled = false, objOverBackground = false)), Some(UByte(2)))
  }

  test("background wins when the object pixel is transparent") {
    assertEquals(emit(MixerCase(objColor = 0)), Some(UByte(1)))
  }

  test("lightest color wins when the background is disabled and the object pixel is transparent") {
    assertEquals(emit(MixerCase(bgEnabled = false, objColor = 0)), Some(UByte(0)))
  }

  test("lightest color wins when the background and objects are disabled") {
    assertEquals(emit(MixerCase(bgEnabled = false, objEnabled = false)), Some(UByte(0)))
  }

  test("reads BGP when the background wins") {
    assertEquals(emit(MixerCase(bgColor = 3, objColor = 0), distinctPalettes), Some(UByte(1)))
  }

  test("reads OBP0 when the object wins on palette 0") {
    assertEquals(emit(MixerCase(), distinctPalettes), Some(UByte(1)))
  }

  test("reads OBP1 when the object wins on palette 1") {
    assertEquals(emit(MixerCase(objUsePalette0 = false), distinctPalettes), Some(UByte(2)))
  }

  test("does not emit when the background FIFO is empty") {
    val state = newState()
    state.objectFifo.enqueue(ObjectPixel(UByte(2)))
    assertEquals(tickOnce(state), None)
  }

  test("does not emit once 160 pixels have been rendered") {
    val state = newState()
    state.pixelMixer.shiftPosition = Ppu.VISIBLE_WIDTH
    state.backgroundFifo.enqueue(BackgroundPixel(UByte(1)))
    state.objectFifo.enqueue(ObjectPixel(UByte(2)))
    assertEquals(tickOnce(state), None)
  }

  test("drops the fine scroll pixels before emitting") {
    val state = newState()
    state.registers(Ppu.Address.SCX) = UByte(3)
    state.pixelMixer.beginScanline(state)
    (0 until 6).foreach(_ => state.backgroundFifo.enqueue(BackgroundPixel(UByte(1))))

    val emittedX = List.newBuilder[Int]
    (0 until 6).foreach(_ => state.pixelMixer.tick(state, (x, _, _) => emittedX += x))

    assertEquals(emittedX.result(), Seq(0, 1, 2))
  }
}

object PixelMixerTests {
  case class Palettes(bgp: UByte, obp0: UByte, obp1: UByte)

  /** Resolves each color index to itself, so an emitted shade reads as the chosen index. */
  val identityPalettes: Palettes = Palettes(UByte(0xE4), UByte(0xE4), UByte(0xE4))

  /** Resolves color index 2 to 0, 1 and 2 respectively, so the shade names the palette read. */
  val distinctPalettes: Palettes = Palettes(UByte(0x4B), UByte(0x1B), UByte(0xE4))

  /**
   * @param bgColor Color index of the background pixel, where 0 is the lightest.
   * @param objColor Color index of the object pixel, where 0 is transparent.
   * @param objOverBackground Object pixel has no BG priority bit set, so it draws over non-lightest BG colors.
   * @param objUsePalette0 Object pixel selects OBP0, otherwise OBP1.
   */
  case class MixerCase(
    bgEnabled: Boolean = true,
    objEnabled: Boolean = true,
    bgColor: Int = 1,
    objColor: Int = 2,
    objOverBackground: Boolean = true,
    objUsePalette0: Boolean = true
  )

  def newState(palettes: Palettes = identityPalettes): State = {
    val state = Ppu.State(emitter = TestEmitters.nullEmitter)
    state.registers(Ppu.Address.BGP) = palettes.bgp
    state.registers(Ppu.Address.OBP0) = palettes.obp0
    state.registers(Ppu.Address.OBP1) = palettes.obp1
    state.lcdControl.bgEnable = true
    state.lcdControl.objEnable = true
    state
  }

  def emit(mixerCase: MixerCase, palettes: Palettes = identityPalettes): Option[UByte] = {
    val state = newState(palettes)
    state.lcdControl.bgEnable = mixerCase.bgEnabled
    state.lcdControl.objEnable = mixerCase.objEnabled
    state.backgroundFifo.enqueue(BackgroundPixel(UByte(mixerCase.bgColor)))
    state.objectFifo.enqueue(
      ObjectPixel(
        UByte(mixerCase.objColor),
        usePalette0 = mixerCase.objUsePalette0,
        backgroundPriority = !mixerCase.objOverBackground
      )
    )
    tickOnce(state)
  }

  def tickOnce(state: State): Option[UByte] = {
    var emitted: Option[UByte] = None
    state.pixelMixer.tick(state, (_, _, color) => emitted = Some(color))
    emitted
  }
}
