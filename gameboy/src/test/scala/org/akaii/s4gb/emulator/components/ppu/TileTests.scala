package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import spire.math.UByte

class TileTests extends FunSuite {

  import TileTests.*

  test("resolve background tile at origin") {
    val state = makeState(scx = 0, scy = 0, ly = 0)
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tilemapBase, Tile.PRIMARY_TILEMAP_ADDRESS)
    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve background tile with fetcherX offset") {
    val state = makeState(scx = 0, scy = 0, ly = 0)
    val expectedX = 3
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = expectedX, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = expectedX, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve background tile with SCX scroll") {
    val state = makeState(scx = 16, scy = 0, ly = 0)
    // SCX=16 -> tile offset = 16/8 = 2, fetcherX=0 -> tilemap x = (2+0)&31 = 2
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = 2, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve background tile with SCY and LY") {
    val state = makeState(scx = 0, scy = 8, ly = 4)
    // y = (4+8)&255 / 8 = 12/8 = 1
    val expectedY = 1
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = 0, y = expectedY)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve background tile wraps fetcherX past 32") {
    val state = makeState(scx = 0, scy = 0, ly = 0)
    // fetcherX=32 -> (0+32)&31 = 0
    val expectedX = 0
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = expectedX, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 32, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve window tile") {
    val state = makeState(ly = 10, wy = 10, wx = 7)
    // windowRowsRendered=0 -> y=0, fetcherX=0 -> x=0
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = true)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve window tile with windowRowsRendered") {
    val state = makeState(ly = 10, wy = 10, wx = 7)
    // windowRowsRendered=16 -> y=16/8=2
    val expectedY = 2
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = 0, y = expectedY)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 16, window = true)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve window tile with fetcherX offset") {
    val state = makeState(ly = 10, wy = 10, wx = 7)
    val expectedX = 5
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = expectedX, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = expectedX, windowRowsRendered = 0, window = true)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve background uses secondary tilemap when bgTileMap is true") {
    val state = makeState(bgTileMap = true)
    val expectedTilemapBase = Tile.SECONDARY_TILEMAP_ADDRESS
    placeTile(state, tileNumber = TEST_TILE_NUMBER, tilemapBase = expectedTilemapBase, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tilemapBase, expectedTilemapBase)
    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("resolve window uses secondary tilemap when windowTileMap is true") {
    val state = makeState(ly = 5, wy = 5, windowTileMap = true)
    val expectedTilemapBase = Tile.SECONDARY_TILEMAP_ADDRESS
    placeTile(state, tileNumber = TEST_TILE_NUMBER, tilemapBase = expectedTilemapBase, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = true)

    assertEquals(tile.tilemapBase, expectedTilemapBase)
    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }

  test("tile data address unsigned mode tile 0 row 0") {
    val state = makeState(bgWindowTileData = true)
    val expectedTileNumber = 0
    val expectedTileDataAddress = 0
    placeTile(state, tileNumber = expectedTileNumber, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileDataAddress, expectedTileDataAddress)
  }

  test("tile data address unsigned mode tile 5 row 3") {
    val state = makeState(bgWindowTileData = true, ly = 3)
    val expectedTileNumber = 5
    // 8 * 2 * 5 + 3 * 2 = 86
    val expectedTileDataAddress = 86
    placeTile(state, tileNumber = expectedTileNumber, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileDataAddress, expectedTileDataAddress)
  }

  test("tile data address signed mode positive tile") {
    val state = makeState(bgWindowTileData = false, ly = 2)
    val expectedTileNumber = 10
    // Base 0x1000 (4096) + 8 * 2 * 10 + 2 * 2 = 4096 + 160 + 4 = 4260
    val expectedTileDataAddress = 4260
    placeTile(state, tileNumber = expectedTileNumber, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileDataAddress, expectedTileDataAddress)
  }

  test("tile data address signed mode negative tile") {
    val state = makeState(bgWindowTileData = false, ly = 0)
    val expectedTileNumber = 200 // unsigned 200 -> signed -56
    // Base 0x1000 (4096) + 8 * 2 * (-56) + 0 * 2 = 4096 - 896 = 3200
    val expectedTileDataAddress = 3200
    placeTile(state, tileNumber = expectedTileNumber, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileDataAddress, expectedTileDataAddress)
  }

  test("tile data address window uses windowRowsRendered for row offset") {
    val state = makeState(bgWindowTileData = true, ly = 0, wy = 0)
    val expectedTileNumber = 3
    // rowOffset = 5 % 8 = 5, 8 * 2 * 3 + 5 * 2 = 58
    val expectedTileDataAddress = 58
    placeTile(state, tileNumber = expectedTileNumber, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 0, windowRowsRendered = 5, window = true)

    assertEquals(tile.tileDataAddress, expectedTileDataAddress)
  }

  test("primary and secondary tilemaps occupy distinct VRAM regions for background tiles") {
    val state = makeState() // bgTileMap defaults to false (primary)
    state.vram(vramIndex(Tile.PRIMARY_TILEMAP_ADDRESS, 0, 0)) = UByte(11)
    state.vram(vramIndex(Tile.SECONDARY_TILEMAP_ADDRESS, 0, 0)) = UByte(22)

    val tile1 = Tile()
    tile1.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)
    assertEquals(tile1.tileNumber, 11)

    state.lcdControl.bgTileMap = true // switch to secondary tilemap
    val tile2 = Tile()
    tile2.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = false)
    assertEquals(tile2.tileNumber, 22)
  }

  test("primary and secondary tilemaps occupy distinct VRAM regions for window tiles") {
    val state = makeState() // windowTileMap defaults to false (primary)
    state.vram(vramIndex(Tile.PRIMARY_TILEMAP_ADDRESS, 0, 0)) = UByte(11)
    state.vram(vramIndex(Tile.SECONDARY_TILEMAP_ADDRESS, 0, 0)) = UByte(22)

    val tile1 = Tile()
    tile1.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = true)
    assertEquals(tile1.tileNumber, 11)

    state.lcdControl.windowTileMap = true // switch to secondary tilemap
    val tile2 = Tile()
    tile2.resolve(state, fetcherX = 0, windowRowsRendered = 0, window = true)
    assertEquals(tile2.tileNumber, 22)
  }

  test("tilemap index wraps x and y within 32x32 tilemap boundary") {
    val state = makeState(scx = 248, scy = 248, ly = 8) // SCX/8 = 31, (SCY+LY)/8 = 32 -> wraps to y=0
    // fetcherX = 1 -> x = (31 + 1) & 31 = 0
    placeTile(state, tileNumber = TEST_TILE_NUMBER, x = 0, y = 0)
    val tile = Tile()
    tile.resolve(state, fetcherX = 1, windowRowsRendered = 0, window = false)

    assertEquals(tile.tileNumber, TEST_TILE_NUMBER)
  }
}

object TileTests extends PixelFetcherFixtures
