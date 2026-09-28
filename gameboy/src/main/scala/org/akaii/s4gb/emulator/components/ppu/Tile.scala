package org.akaii.s4gb.emulator.components.ppu

import spire.math.UByte

/**
 * Mutable tile state, reused across fetcher cycles to avoid allocation.
 * Not a case class — resolve updates fields in place.
 *
 * @see [[https://gbdev.io/pandocs/Tile_Data.html]]
 * @see [[https://gbdev.io/pandocs/Tile_Maps.html]]
 */
class Tile {
  var tilemapBase: Int = 0
  var tileNumber: Int = 0
  var tileDataAddress: Int = 0
  var tileDataLow: UByte = UByte(0)
  var tileDataHigh: UByte = UByte(0)

  def resolve(state: Ppu.State, fetcherX: Int, windowRowsRendered: Int, window: Boolean): Unit = {
    val lcdc = state.lcdControl
    val scx = state.registers(Ppu.Address.SCX).toInt
    val scy = state.registers(Ppu.Address.SCY).toInt
    val currentScanline = state.ly.toInt

    // Objects always use “$8000 addressing”, but the BG and Window can use either mode, controlled by LCDC bit 4.
    // @see [[https://gbdev.io/pandocs/Tile_Data.html]]
    if (window) {
      val wrappedX = wrap(fetcherX)
      val wrappedY = wrap(windowRowsRendered / Tile.SIZE)
      tilemapBase = if (lcdc.windowTileMap) Tile.SECONDARY_TILEMAP_ADDRESS else Tile.PRIMARY_TILEMAP_ADDRESS
      tileNumber = state.vram(tilemapIndex(wrappedX, wrappedY)).toInt
      tileDataAddress = computeTileDataAddress(tileNumber, windowRowsRendered % Tile.SIZE, lcdc.bgWindowTileData)
    } else {
      val wrappedX = wrap((scx / Tile.SIZE) + fetcherX)
      val wrappedY = wrap(((currentScanline + scy) & Tile.COORDINATE_MASK) / Tile.SIZE)
      tilemapBase = if (lcdc.bgTileMap) Tile.SECONDARY_TILEMAP_ADDRESS else Tile.PRIMARY_TILEMAP_ADDRESS
      tileNumber = state.vram(tilemapIndex(wrappedX, wrappedY)).toInt
      tileDataAddress = computeTileDataAddress(tileNumber, (currentScanline + scy) % Tile.SIZE, lcdc.bgWindowTileData)
    }
  }

  /**
   * Wrapping the visible area of the background
   * @see [[https://gbdev.io/pandocs/Tile_Maps.html#background-bg]]
   */
  @inline private def wrap(v: Int): Int =
    v & Tile.TILEMAP_DIMENSION_MASK

  /**
   * Index computation based on current values
   * @see [[https://gbdev.io/pandocs/Tile_Maps.html#tile-indexes]]
   */
  @inline private def tilemapIndex(x: Int, y: Int): Int =
    tilemapBase + (y * Tile.TILES_PER_ROW + x)

  /**
   * Tile number signed/unsigned addressing
   * @see [[https://gbdev.io/pandocs/Tile_Data.html]]
   */
  @inline private def computeTileDataAddress(tileNumber: Int, rowOffset: Int, unsignedMode: Boolean): Int = {
    val bytesPerRow = Tile.BYTES_PER_ROW
    val rowBytes = Tile.SIZE * bytesPerRow
    if (unsignedMode) {
      // $8000 mode: base 0x0000, unsigned tileNumber 0 to 255
      tileNumber * rowBytes + rowOffset * bytesPerRow
    } else {
      // $8800 mode: base 0x1000 ($9000 in VRAM), signed tileNumber -128 to 127
      val signedTile = if (tileNumber > Tile.MAX_SIGNED_TILE_NUMBER) tileNumber - Tile.UNSIGNED_TO_SIGNED_OFFSET else tileNumber
      Tile.SIGNED_BASE_ADDRESS + (signedTile * rowBytes) + (rowOffset * bytesPerRow)
    }
  }
}

object Tile {
  val SIZE: Int = 8
  private val TILES_PER_ROW: Int = 32
  private val TILEMAP_DIMENSION_MASK = TILES_PER_ROW - 1 // 32x32 tilemap wrapping mask (0x1F)
  private val COORDINATE_MASK: Int = 0xFF
  private[ppu] val PRIMARY_TILEMAP_ADDRESS: Int = 0x1800 // $9800-$9BFF
  private[ppu] val SECONDARY_TILEMAP_ADDRESS: Int = 0x1C00 // $9C00-$9FFF
  private[ppu] val SIGNED_BASE_ADDRESS: Int = 0x1000 // $9000 in VRAM space
  private val MAX_SIGNED_TILE_NUMBER: Int = 127
  private val UNSIGNED_TO_SIGNED_OFFSET: Int = 256
  private val BYTES_PER_ROW: Int = 2

  def apply(): Tile = new Tile
}
