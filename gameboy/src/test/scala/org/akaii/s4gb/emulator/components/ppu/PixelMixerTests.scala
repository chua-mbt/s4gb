package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.emulator.components.ppu.Ppu.State
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte
import PixelMixerTests.*

class PixelMixerTests extends FunSuite {
  import PixelMixerTests.*

  test("PixelMixer should emit object color when BG enabled, OBJ enabled, and OBJ pixel is opaque") {
    val result = runTest()
    assertEquals(result.emittedColor, Some(objDefaultColor))
  }

  test("PixelMixer should emit background color when BG enabled and OBJ disabled") {
    val result = runTest(configureState = _.lcdControl.objEnable = false)
    assertEquals(result.emittedColor, Some(bgDefaultColor))
  }

  test("PixelMixer should emit object color when BG disabled, OBJ enabled, and OBJ pixel is opaque") {
    val result = runTest(configureState = s => s.lcdControl.bgEnable = false)
    assertEquals(result.emittedColor, Some(objDefaultColor))
  }

  test("PixelMixer should emit white color (color 0) when BG disabled and OBJ disabled") {
    val result = runTest(configureState = s => {
      s.lcdControl.bgEnable = false
      s.lcdControl.objEnable = false
    })
    assertEquals(result.emittedColor, Some(BackgroundPixel.lightestColor))
  }

  test("PixelMixer should emit background color when OBJ pixel is transparent") {
    val result = runTest(configureOBJ = _.colorIndex = ObjectPixel.transparentColor)
    assertEquals(result.emittedColor, Some(bgDefaultColor))
  }

  test("PixelMixer should emit object color when BG pixel is transparent") {
    val result = runTest(configureBG = _.colorIndex = BackgroundPixel.lightestColor)
    assertEquals(result.emittedColor, Some(objDefaultColor))
  }

  test("PixelMixer should emit background color when OBJ pixel has priority blocked (isOverBackground=false)") {
    val result = runTest(configureOBJ = _.backgroundPriority = true)
    assertEquals(result.emittedColor, Some(bgDefaultColor))
  }

  test("PixelMixer should emit object color when OBJ pixel has priority set and BG is not transparent") {
    val result = runTest(configureOBJ = _.backgroundPriority = false)
    assertEquals(result.emittedColor, Some(objDefaultColor))
  }

  test("PixelMixer should emit object color when BG color is 0 and OBJ pixel has priority blocked (DMG rule)") {
    val result = runTest(
      configureBG = _.colorIndex = BackgroundPixel.lightestColor,
      configureOBJ = _.backgroundPriority = true
    )
    assertEquals(result.emittedColor, Some(objDefaultColor))
  }

  test("PixelMixer should use OBP0 correctly") {
    val palette0 = UByte(0xE4)
    val colorIndex = UByte(2)
    val expectedColor = UByte(2) // 0xEF, bits 5-4 = 0b10 = shade 2

    val state = Ppu.State()
    state.lcdControl.bgEnable = true
    state.lcdControl.objEnable = true
    state.registers(Ppu.Address.OBP0) = palette0
    state.backgroundFifo.enqueue(BackgroundPixel(BackgroundPixel.lightestColor))
    state.objectFifo.enqueue(ObjectPixel(colorIndex, usePalette0 = true))

    var emitted: Option[UByte] = None
    state.pixelMixer.tick(state, (_, _, c) => emitted = Some(c))

    assertEquals(emitted, Some(expectedColor))
  }

  test("PixelMixer should use OBP1 correctly") {
    val palette1 = UByte(0x10)
    val colorIndex = UByte(2)
    val expectedColor = UByte(1) // 0x10, bits 5-4 = 0b01 = shade 1

    val state = Ppu.State()
    state.lcdControl.bgEnable = true
    state.lcdControl.objEnable = true
    state.registers(Ppu.Address.OBP1) = palette1
    state.backgroundFifo.enqueue(BackgroundPixel(BackgroundPixel.lightestColor))
    state.objectFifo.enqueue(ObjectPixel(colorIndex, usePalette0 = false))

    var emitted: Option[UByte] = None
    state.pixelMixer.tick(state, (_, _, c) => emitted = Some(c))

    assertEquals(emitted, Some(expectedColor))
  }

  test("PixelMixer should not emit when FIFO conditions are not met") {
    val stateEmpty = Ppu.State()
    var emitted = false
    val emitPixel: PixelEmitter =  (_, _, _) => emitted = true
    stateEmpty.pixelMixer.tick(stateEmpty, emitPixel)
    assert(!emitted)

    val stateOnlyBg = Ppu.State()
    stateOnlyBg.backgroundFifo.enqueue(BackgroundPixel(bgDefaultColor))
    stateOnlyBg.pixelMixer.tick(stateOnlyBg, emitPixel)
    assert(!emitted)

    val stateOnlyObj = Ppu.State()
    stateOnlyObj.objectFifo.enqueue(ObjectPixel(objDefaultColor))
    stateOnlyObj.pixelMixer.tick(stateOnlyObj, emitPixel)
    assert(!emitted)
  }

  test("PixelMixer should not emit when scanline limit (160 dots) is reached") {
    val state = Ppu.State()
    state.scanlineDot.current = 160
    state.backgroundFifo.enqueue(BackgroundPixel(bgDefaultColor))
    state.objectFifo.enqueue(ObjectPixel(objDefaultColor))
    
    var emitted = false
    state.pixelMixer.tick(state, (_, _, _) => emitted = true)
    assert(!emitted)
  }
}

object PixelMixerTests {
  val bgDefaultColor: UByte = UByte(1)
  val objDefaultColor: UByte = UByte(2)

  case class TestResult(emittedColor: Option[UByte])

  def runTest(
    configureState: State => Unit = _ => (),
    configureBG: BackgroundPixel => Unit = _ => (),
    configureOBJ: ObjectPixel => Unit = _ => (),
    bgPixel: BackgroundPixel = BackgroundPixel(bgDefaultColor),
    objPixel: ObjectPixel = ObjectPixel(objDefaultColor)
  ): TestResult = {
    val state = Ppu.State()
    
    // Set identity mapping for palettes: 0->0, 1->1, 2->2, 3->3
    // 0xE4 = 11 10 01 00 (binary)
    val identityPalette = UByte(0xE4)
    state.registers(Ppu.Address.BGP) = identityPalette
    state.registers(Ppu.Address.OBP0) = identityPalette
    state.registers(Ppu.Address.OBP1) = identityPalette
    
    // Default to enabled
    state.lcdControl.bgEnable = true
    state.lcdControl.objEnable = true
    
    configureState(state)
    configureBG(bgPixel)
    configureOBJ(objPixel)
    
    state.backgroundFifo.enqueue(bgPixel)
    state.objectFifo.enqueue(objPixel)
    
    var result: Option[UByte] = None
    state.pixelMixer.tick(state, (x: Int, y: Int, c: UByte) => result = Some(c))
    
    TestResult(result)
  }
}
