package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import spire.math.UByte

class GameboyObjectTests extends FunSuite {

  test("attribute accessors decode each bit of OAM byte 3") {
    val none = GameboyObject(attributes = UByte(0x00))
    assertEquals(none.backgroundPriority, false)
    assertEquals(none.yFlipped, false)
    assertEquals(none.xFlipped, false)
    assertEquals(none.usePalette0, true)

    assertEquals(GameboyObject(attributes = UByte(0x80)).backgroundPriority, true)
    assertEquals(GameboyObject(attributes = UByte(0x40)).yFlipped, true)
    assertEquals(GameboyObject(attributes = UByte(0x20)).xFlipped, true)
    assertEquals(GameboyObject(attributes = UByte(0x10)).usePalette0, false)
  }

  test("attribute accessors all read as set when byte 3 is $F0") {
    val obj = GameboyObject(attributes = UByte(0xF0))

    assertEquals(obj.backgroundPriority, true)
    assertEquals(obj.yFlipped, true)
    assertEquals(obj.xFlipped, true)
    assertEquals(obj.usePalette0, false)
  }

  test("topScreenRow removes the 16 pixel OAM bias") {
    assertEquals(GameboyObject.topScreenRow(0), -16)
    assertEquals(GameboyObject.topScreenRow(16), 0)
    assertEquals(GameboyObject.topScreenRow(30), 14)
    assertEquals(GameboyObject.topScreenRow(152), 136)
  }

  test("lineForScreenRow translates a screen row into object space") {
    val obj = GameboyObject(y = UByte(30))

    assertEquals(obj.lineForScreenRow(ly = 14, height = 8), 0)
    assertEquals(obj.lineForScreenRow(ly = 20, height = 8), 6)
  }

  test("lineForScreenRow mirrors the row when Y flipped") {
    val obj = GameboyObject(y = UByte(30), attributes = UByte(0x40))

    assertEquals(obj.lineForScreenRow(ly = 14, height = 8), 7)
    assertEquals(obj.lineForScreenRow(ly = 20, height = 8), 1)
  }

  test("lineForScreenRow mirrors across the whole height of an 8x16 object") {
    val obj = GameboyObject(y = UByte(16), attributes = UByte(0x40))

    assertEquals(obj.lineForScreenRow(ly = 0, height = 16), 15)
    assertEquals(obj.lineForScreenRow(ly = 8, height = 16), 7)
    assertEquals(obj.lineForScreenRow(ly = 15, height = 16), 0)
  }

  test("tileNumberFor returns the OAM tile index unchanged in 8x8 mode") {
    val obj = GameboyObject(tileIndex = UByte(5))

    assertEquals(obj.tileNumberFor(lineInObject = 0, tall = false), 5)
    assertEquals(obj.tileNumberFor(lineInObject = 7, tall = false), 5)
  }

  test("tileNumberFor ignores the low bit and picks the top tile in 8x16 mode") {
    val obj = GameboyObject(tileIndex = UByte(5))

    assertEquals(obj.tileNumberFor(lineInObject = 0, tall = true), 4)
    assertEquals(obj.tileNumberFor(lineInObject = 7, tall = true), 4)
  }

  test("tileNumberFor picks the bottom tile in 8x16 mode") {
    val obj = GameboyObject(tileIndex = UByte(5))

    assertEquals(obj.tileNumberFor(lineInObject = 8, tall = true), 5)
    assertEquals(obj.tileNumberFor(lineInObject = 15, tall = true), 5)
  }

  test("set stores the OAM bytes and marks the object in use") {
    val obj = GameboyObject()
    assertEquals(obj.notInUse, true)

    obj.set(y = UByte(10), x = UByte(20), tileIndex = UByte(30), attributes = UByte(40))

    assertEquals(obj.y, UByte(10))
    assertEquals(obj.x, UByte(20))
    assertEquals(obj.tileIndex, UByte(30))
    assertEquals(obj.attributes, UByte(40))
    assertEquals(obj.notInUse, false)
  }

  test("reset marks the object unused without clearing the bytes") {
    val obj = GameboyObject()
    obj.set(y = UByte(10), x = UByte(20), tileIndex = UByte(30), attributes = UByte(40))

    obj.reset()

    assertEquals(obj.notInUse, true)
    assertEquals(obj.y, UByte(10))
    assertEquals(obj.x, UByte(20))
  }
}
