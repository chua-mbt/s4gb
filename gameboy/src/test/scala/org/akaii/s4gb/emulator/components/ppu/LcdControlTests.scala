package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import spire.math.UByte

class LcdControlTests extends FunSuite {

  test("initial state") {
    val lcdc = LcdControl()

    assertEquals(lcdc.lcdEnable, false)
    assertEquals(lcdc.windowTileMap, false)
    assertEquals(lcdc.windowEnable, false)
    assertEquals(lcdc.bgWindowTileData, false)
    assertEquals(lcdc.bgTileMap, false)
    assertEquals(lcdc.objSize, false)
    assertEquals(lcdc.objEnable, false)
    assertEquals(lcdc.bgEnable, false)
    assertEquals(lcdc.read(), UByte(0x00))
  }

  test("initialize resets to power-up state") {
    val lcdc = LcdControl()

    lcdc.lcdEnable = false
    lcdc.windowEnable = true
    lcdc.bgEnable = false

    lcdc.initialize()

    assertEquals(lcdc.lcdEnable, true)
    assertEquals(lcdc.windowTileMap, false)
    assertEquals(lcdc.windowEnable, false)
    assertEquals(lcdc.bgWindowTileData, true)
    assertEquals(lcdc.bgTileMap, false)
    assertEquals(lcdc.objSize, false)
    assertEquals(lcdc.objEnable, false)
    assertEquals(lcdc.bgEnable, true)

    assertEquals(lcdc.read(), UByte(0x91))
  }

  test("write updates control bits") {
    val lcdc = LcdControl()
    lcdc.initialize()

    lcdc.write(UByte(0xFF))

    assertEquals(lcdc.lcdEnable, true)
    assertEquals(lcdc.windowTileMap, true)
    assertEquals(lcdc.windowEnable, true)
    assertEquals(lcdc.bgWindowTileData, true)
    assertEquals(lcdc.bgTileMap, true)
    assertEquals(lcdc.objSize, true)
    assertEquals(lcdc.objEnable, true)
    assertEquals(lcdc.bgEnable, true)

    assertEquals(lcdc.read(), UByte(0xFF))
  }

  test("write clears bits correctly") {
    val lcdc = LcdControl()
    lcdc.initialize()

    lcdc.write(UByte(0x00))

    assertEquals(lcdc.lcdEnable, false)
    assertEquals(lcdc.windowTileMap, false)
    assertEquals(lcdc.windowEnable, false)
    assertEquals(lcdc.bgWindowTileData, false)
    assertEquals(lcdc.bgTileMap, false)
    assertEquals(lcdc.objSize, false)
    assertEquals(lcdc.objEnable, false)
    assertEquals(lcdc.bgEnable, false)

    assertEquals(lcdc.read(), UByte(0x00))
  }

  test("round trip write then read") {
    val cases = Seq(
      0x00,
      0x01,
      0x08,
      0x10,
      0x20,
      0x40,
      0x80,
      0xFF
    )

    cases.foreach { v =>
      val lcdc = LcdControl()
      lcdc.initialize()

      lcdc.write(UByte(v))
      assertEquals(lcdc.read(), UByte(v))
    }
  }

  test("bit masks behave correctly") {
    val lcdc = LcdControl()

    lcdc.write(UByte(0x01))
    assertEquals(lcdc.bgEnable, true)

    lcdc.write(UByte(0x02))
    assertEquals(lcdc.objEnable, true)

    lcdc.write(UByte(0x80))
    assertEquals(lcdc.lcdEnable, true)

    lcdc.write(UByte(0x00))
    assertEquals(lcdc.lcdEnable, false)
  }

}