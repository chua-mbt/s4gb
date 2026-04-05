package org.akaii.s4gb.emulator.components.ppu

import munit.*
import org.akaii.s4gb.collections.RingBuffer
import spire.math.{UByte, UShort}

import scala.collection.mutable

class OamScannerTests extends FunSuite {

  import OamScannerTests.*

  test("identifies 5 objects in given ly when 35 other objects are not in ly") {
    val (state, oam) = createStateWithOam(40)

    val sprite = GameboyObject(y = UByte(66), x = UByte(8), tileIndex = UByte(1), attributes = UByte(2))
    writeSprites(oam, 0 to 4, sprite)
    writeSprites(oam, 5 until 40, GameboyObject(y = UByte(10)))

    (0 to PpuMode.OamScan.END_DOT by 2).foreach { dot =>
      state.scanlineDot.current = dot
      OamScanner.scan(state)
    }

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 5)

    objectsInScanline.foreach(assertObjectEquals(_, sprite))
  }

  test("limits to first 10 objects when more than 10 are in the ly") {
    val (state, oam) = createStateWithOam(40)

    val visibleSprite = GameboyObject(y = UByte(66))
    (0 until 12).foreach { i =>
      writeSprite(oam, i, visibleSprite.copy(x = UByte(i + 1)))
    }

    (0 to PpuMode.OamScan.END_DOT by 2).foreach { dot =>
      state.scanlineDot.current = dot
      OamScanner.scan(state)
    }

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 10)

    objectsInScanline.zipWithIndex.foreach { case (obj, i) =>
      assertEquals(obj.x, UByte(i + 1), s"Object $i x should be ${i + 1}")
    }
  }

  test("ignores objects not in current scanline") {
    val (state, oam) = createStateWithOam(2)

    val visibleSprite = GameboyObject(y = UByte(66), x = UByte(10), tileIndex = UByte(5), attributes = UByte(3))
    writeSprite(oam, 0, visibleSprite)
    writeSprite(oam, 1, GameboyObject(y = UByte(10)))

    state.scanlineDot.current = 0
    OamScanner.scan(state)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 1)
    assertObjectEquals(objectsInScanline(0), visibleSprite)
  }

  test("8px sprites - sprite visible when Y allows 8 pixel height") {
    val (state, oam) = createStateWithOam(1, is8Px = true)
    writeSprite(oam, 0, GameboyObject(y = UByte(66)))

    state.scanlineDot.current = 0
    OamScanner.scan(state)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 1)
    assertEquals(objectsInScanline(0).y, UByte(66))
  }

  test("8px sprites - sprite not visible when Y requires more than 8 pixels") {
    val (state, oam) = createStateWithOam(1, is8Px = true)
    writeSprite(oam, 0, GameboyObject(y = UByte(58)))

    state.scanlineDot.current = 0
    OamScanner.scan(state)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 0)
  }

  test("16px sprites - sprite visible when Y allows 16 pixel height") {
    val (state, oam) = createStateWithOam(1, is8Px = false)
    writeSprite(oam, 0, GameboyObject(y = UByte(66)))

    state.scanlineDot.current = 0
    OamScanner.scan(state)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 1)
    assertEquals(objectsInScanline(0).y, UByte(66))
  }

  test("16px sprites - sprite at upper boundary is visible") {
    val (state, oam) = createStateWithOam(1, is8Px = false, ly = UByte(57))
    writeSprite(oam, 0, GameboyObject(y = UByte(66)))

    state.scanlineDot.current = 0
    OamScanner.scan(state)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 1)
    assertEquals(objectsInScanline(0).y, UByte(66))
  }

  test("finds exactly 10 objects with 10 sprites in ly") {
    val (state, oam) = createStateWithOam()
    writeSprites(oam, 0 until 10, GameboyObject(y = UByte(66)))

    (0 to PpuMode.OamScan.END_DOT by 2).foreach { dot =>
      state.scanlineDot.current = dot
      OamScanner.scan(state)
    }

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 10)
  }

  test("empty oam finds no objects") {
    val (state, _) = createStateWithOam()

    state.scanlineDot.current = 0
    OamScanner.scan(state)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 0)
  }

  private def assertObjectEquals(actual: GameboyObject, expected: GameboyObject): Unit = {
    assertEquals(actual.y, expected.y)
    assertEquals(actual.x, expected.x)
    assertEquals(actual.tileIndex, expected.tileIndex)
    assertEquals(actual.attributes, expected.attributes)
  }
}

object OamScannerTests {
  private val OAM_BYTE_SIZE = 4
  private val SPRITES_IN_OAM = 40

  def createStateWithOam(
    numSprites: Int = SPRITES_IN_OAM,
    ly: UByte = UByte(50),
    is8Px: Boolean = true
  ): (Ppu.State, Array[UByte]) = {
    val oam = Array.fill(numSprites * OAM_BYTE_SIZE)(UByte(0))
    val lcdc = if (is8Px) UByte(0x00) else UByte(0x04)
    val registers = mutable.Map[UShort, UByte](Ppu.Address.LCDC -> lcdc)
    val state = Ppu.State(
      oam,
      Array.fill(Ppu.VRAM_SIZE)(UByte(0)),
      registers,
      Array.fill(OamScanner.OBJECTS_PER_SCANLINE)(GameboyObject()),
      RingBuffer[Pixel](Ppu.FIFO_SIZE),
      RingBuffer[Pixel](Ppu.FIFO_SIZE),
      ScanlineDot(),
      LcdStatus(),
      LcdControl(),
      ly,
    )
    (state, oam)
  }

  def writeSprite(oam: Array[UByte], index: Int, sprite: GameboyObject): Unit = {
    val offset = index * OAM_BYTE_SIZE
    oam(offset) = sprite.y
    oam(offset + 1) = sprite.x
    oam(offset + 2) = sprite.tileIndex
    oam(offset + 3) = sprite.attributes
  }

  def writeSprites(oam: Array[UByte], range: Range, sprite: GameboyObject): Unit =
    range.foreach(i => writeSprite(oam, i, sprite))
}
