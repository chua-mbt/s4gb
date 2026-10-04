package org.akaii.s4gb.emulator.components.ppu

import munit.{Assertions, FunSuite}
import spire.math.UByte

class OamScannerTests extends FunSuite with TestEmitters {

  import OamScannerTests.*

  test("identifies 5 objects in given ly when 35 other objects are not in ly") {
    val (state, oam) = createStateWithOam()

    val gbObject = GameboyObject(y = UByte(66), x = UByte(8), tileIndex = UByte(1), attributes = UByte(2))
    writeGbObjects(oam, 0 to 4, gbObject)
    writeGbObjects(oam, 5 until 40, GameboyObject(y = UByte(10)))

    scanScanline(state, oam)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 5)

    objectsInScanline.foreach(assertObjectEquals(_, gbObject))
  }

  test("limits to first 10 objects when more than 10 are in the ly") {
    val (state, oam) = createStateWithOam()

    val visibleGbObject = GameboyObject(y = UByte(66))
    (0 until 12).foreach { i =>
      writeGbObject(oam, i, visibleGbObject.copy(x = UByte(i + 1)))
    }

    scanScanline(state, oam)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 10)

    objectsInScanline.zipWithIndex.foreach { case (gbObject, i) =>
      assertEquals(gbObject.x, UByte(i + 1), s"Object $i x should be ${i + 1}")
    }
  }

  test("ignores objects not in current scanline") {
    val (state, oam) = createStateWithOam()

    val visibleGbObject = GameboyObject(y = UByte(66), x = UByte(10), tileIndex = UByte(5), attributes = UByte(3))
    writeGbObject(oam, 0, visibleGbObject)
    writeGbObject(oam, 1, GameboyObject(y = UByte(10)))

    scanScanline(state, oam)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 1)
    assertObjectEquals(objectsInScanline(0), visibleGbObject)
  }

  test("8px object visible when Y allows 8 pixel height") {
    val (state, oam) = createStateWithOam(1, is8Px = true)
    writeGbObject(oam, 0, GameboyObject(y = UByte(66)))

    scanScanline(state, oam)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 1)
    assertEquals(objectsInScanline(0).y, UByte(66))
  }

  test("8px object not visible when Y requires more than 8 pixels") {
    val (state, oam) = createStateWithOam(1, is8Px = true)
    writeGbObject(oam, 0, GameboyObject(y = UByte(58)))

    scanScanline(state, oam)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 0)
  }

  test("16px object visible when Y allows 16 pixel height") {
    val (state, oam) = createStateWithOam(1, is8Px = false, ly = UByte(57))
    writeGbObject(oam, 0, GameboyObject(y = UByte(66)))

    scanScanline(state, oam)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 1)
    assertEquals(objectsInScanline(0).y, UByte(66))
  }

  test("empty oam finds no objects") {
    val (state, oam) = createStateWithOam()

    scanScanline(state, oam)

    val objectsInScanline = state.scanlineObjects.filter(!_.notInUse)
    assertEquals(objectsInScanline.length, 0)
  }

  test("sortByDrawingPriority puts the objects into left to right fetch order") {
    val (state, oam) = createStateWithOam()
    // scanned in OAM order, so the buffer holds them in that order first
    writeGbObject(oam, 0, GameboyObject(y = UByte(66), x = UByte(40), tileIndex = UByte(1)))
    writeGbObject(oam, 1, GameboyObject(y = UByte(66), x = UByte(8), tileIndex = UByte(2)))
    writeGbObject(oam, 2, GameboyObject(y = UByte(66), x = UByte(24), tileIndex = UByte(3)))
    scanScanline(state, oam)

    OamScanner.sortByDrawingPriority(state)

    assertEquals(state.scanlineObjects.take(3).map(_.tileIndex.toInt).toSeq, Seq(2, 3, 1))
  }

  test("sortByDrawingPriority breaks ties on X by OAM order") {
    val (state, oam) = createStateWithOam()
    writeGbObject(oam, 0, GameboyObject(y = UByte(66), x = UByte(24), tileIndex = UByte(1)))
    writeGbObject(oam, 1, GameboyObject(y = UByte(66), x = UByte(24), tileIndex = UByte(2)))
    writeGbObject(oam, 2, GameboyObject(y = UByte(66), x = UByte(24), tileIndex = UByte(3)))
    scanScanline(state, oam)

    OamScanner.sortByDrawingPriority(state)

    assertEquals(state.scanlineObjects.take(3).map(_.tileIndex.toInt).toSeq, Seq(1, 2, 3))
  }

  test("sortByDrawingPriority leaves the unused slots after the found ones") {
    val (state, oam) = createStateWithOam()
    writeGbObject(oam, 0, GameboyObject(y = UByte(66), x = UByte(40), tileIndex = UByte(1)))
    writeGbObject(oam, 1, GameboyObject(y = UByte(10), x = UByte(40), tileIndex = UByte(2)))
    scanScanline(state, oam)

    OamScanner.sortByDrawingPriority(state)

    assertEquals(state.scanlineObjects.head.tileIndex.toInt, 1)
    assertEquals(state.scanlineObjects(1).notInUse, true)
    assertEquals(state.scanlineObjects(2).notInUse, true)
  }

  test("sortByDrawingPriority reverses a line that arrives in descending X") {
    val (state, oam) = createStateWithOam()
    writeGbObject(oam, 0, GameboyObject(y = UByte(66), x = UByte(40), tileIndex = UByte(1)))
    writeGbObject(oam, 1, GameboyObject(y = UByte(66), x = UByte(24), tileIndex = UByte(2)))
    writeGbObject(oam, 2, GameboyObject(y = UByte(66), x = UByte(8), tileIndex = UByte(3)))
    scanScanline(state, oam)

    OamScanner.sortByDrawingPriority(state)

    assertEquals(state.scanlineObjects.take(3).map(_.tileIndex.toInt).toSeq, Seq(3, 2, 1))
  }

  test("sortByDrawingPriority sorts a line with all ten slots filled") {
    val (state, oam) = createStateWithOam()
    val xByOamIndex = Seq(120, 8, 72, 24, 160, 40, 96, 16, 136, 56)
    xByOamIndex.zipWithIndex.foreach { case (x, index) =>
      writeGbObject(oam, index, GameboyObject(y = UByte(66), x = UByte(x), tileIndex = UByte(index + 1)))
    }
    scanScanline(state, oam)

    OamScanner.sortByDrawingPriority(state)

    assertEquals(state.scanlineObjects.map(_.tileIndex.toInt).toSeq, Seq(2, 8, 4, 6, 10, 3, 7, 1, 9, 5))
  }

}

object OamScannerTests extends Assertions {
  private val OBJECTS_IN_OAM: Int = 40

  def createStateWithOam(
    numObjects: Int = OBJECTS_IN_OAM,
    ly: UByte = UByte(50),
    is8Px: Boolean = true
  ): (Ppu.State, Array[UByte]) = {
    val oam = Array.fill(numObjects * GameboyObject.BYTE_SIZE)(UByte(0))
    val state = Ppu.State(
      emitter = TestEmitters.nullEmitter,
      oam = oam,
      vram = Array.fill(Ppu.VRAM_SIZE)(UByte(0)),
      lcdControl = LcdControl(objSize = !is8Px),
      ly = ly,
    )
    (state, oam)
  }

  def writeGbObject(oam: Array[UByte], index: Int, gbObject: GameboyObject): Unit = {
    val offset = index * GameboyObject.BYTE_SIZE
    oam(offset) = gbObject.y
    oam(offset + 1) = gbObject.x
    oam(offset + 2) = gbObject.tileIndex
    oam(offset + 3) = gbObject.attributes
  }

  def writeGbObjects(oam: Array[UByte], range: Range, gbObject: GameboyObject): Unit =
    range.foreach(i => writeGbObject(oam, i, gbObject))

  def scanScanline(state: Ppu.State, oam: Array[UByte]): Unit = {
    val slots = oam.length / GameboyObject.BYTE_SIZE
    (0 until slots * OamScanner.DOTS_PER_SCAN by OamScanner.DOTS_PER_SCAN).foreach { dot =>
      state.scanlineDot.current = dot
      OamScanner.scan(state)
    }
  }

  def assertObjectEquals(actual: GameboyObject, expected: GameboyObject): Unit = {
    assertEquals(actual.y, expected.y)
    assertEquals(actual.x, expected.x)
    assertEquals(actual.tileIndex, expected.tileIndex)
    assertEquals(actual.attributes, expected.attributes)
  }
}
