package org.akaii.s4gb.emulator.components

import org.akaii.s4gb.emulator.byteops.*
import org.akaii.s4gb.emulator.memorymap.RegisterMap
import spire.math.{UByte, UShort}

import scala.collection.mutable

/**
 * Picture Processing Unit
 *
 * @see [[https://gbdev.io/pandocs/Rendering.html#ppu-modes]]
 */
case class Ppu(
  vram: Array[UByte] = Array.fill(Ppu.VRAM_SIZE)(UByte(0)),
  oam: Array[UByte] = Array.fill(Ppu.OAM_SIZE)(UByte(0))
) extends RegisterMap {

  import Ppu.*
  import Ppu.Address.*

  private var currentMode: Mode = Mode.OamScan

  /**
   * Initialize hardware registers with their DMG boot values.
   */
  override protected val registers: mutable.Map[UShort, UByte] = mutable.Map(
    LCDC -> UByte(0),
    STAT -> UByte(0),
    SCY  -> UByte(0),
    SCX  -> UByte(0),
    LY   -> UByte(0),
    LYC  -> UByte(0),
    BGP  -> UByte(0),
    OBP0 -> UByte(0),
    OBP1 -> UByte(0),
    WY   -> UByte(0),
    WX   -> UByte(0)
  )

  /**
   * Initializes registers to DMG power-up state.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html#hardware-registers]]
   */
  def initialize(): Unit = {
    registers(LCDC) = UByte(0x91)
    registers(STAT) = UByte(0x85)
    registers(SCY)  = UByte(0x00)
    registers(SCX)  = UByte(0x00)
    registers(LY)   = UByte(0x00)
    registers(LYC)  = UByte(0x00)
    registers(BGP)  = UByte(0xFC)
    registers(OBP0) = UByte(0xFF)
    registers(OBP1) = UByte(0xFF)
    registers(WY)   = UByte(0x00)
    registers(WX)   = UByte(0x00)
  }

  override def apply(address: UShort): UByte = {
    if (isVram(address)) {
      vram(vramIndex(address))
    } else if (isOam(address)) {
      oam(oamIndex(address))
    } else if (address == STAT) {
      ???
    } else {
      super.apply(address)
    }
  }

  override def write(address: UShort, value: UByte): Unit = {
    if (isVram(address)) {
      vram(vramIndex(address)) = value
    } else if (isOam(address)) {
      oam(oamIndex(address)) = value
    } else if (address == LY) {
      () // LY is read-only
    } else if (address == STAT) {
      // Bits 0-2 are read-only; only bits 3-6 are writable
      val writableMask = UByte(0x78)
      val currentVal = registers(STAT)
      registers(STAT) = (currentVal & ~writableMask) | (value & writableMask)
    } else {
      super.write(address, value)
    }
  }

  def tick(): Unit = ???

  @inline private def isVram(address: UShort): Boolean = address >= VRAM.START && address <= VRAM.END
  @inline private def isOam(address: UShort): Boolean = address >= OAM.START && address <= OAM.END

  @inline private def vramIndex(address: UShort): Int = address.toInt - VRAM.START.toInt
  @inline private def oamIndex(address: UShort): Int = address.toInt - OAM.START.toInt
}

object Ppu {
  /**
   * PPU Modes
   *
   * @see [[https://gbdev.io/pandocs/Rendering.html?highlight=mode#ppu-modes]]
   */
  enum Mode(val statValue: UByte) {
    case HorizontalBlank extends Mode(UByte(0x00))
    case VerticalBlank extends Mode(UByte(0x01))
    case OamScan extends Mode(UByte(0x02))
    case Draw extends Mode(UByte(0x03))
  }

  object Address {

    /**
     * VRAM (Video RAM)
     *
     * @see [[https://gbdev.io/pandocs/Memory_Map.html#vram-memory-map]]
     * @see [[https://gbdev.io/pandocs/Tile_Data.html?highlight=vram%208000#vram-tile-data]]
     * @see [[https://gbdev.io/pandocs/Accessing_VRAM_and_OAM.html?highlight=vram%208000#accessing-vram-and-oam]]
     */
    object VRAM {
      val START: UShort = UShort(0x8000)
      val END: UShort = UShort(0x9FFF)
    }

    /**
     * OAM (Object Attribute Memory)
     *
     * @see [[https://gbdev.io/pandocs/Tile_Data.html?highlight=vram%208000#vram-tile-data]]
     * @see [[https://gbdev.io/pandocs/Accessing_VRAM_and_OAM.html?highlight=vram%208000#accessing-vram-and-oam]]
     */
    object OAM {
      val START: UShort = UShort(0xFE00)
      val END: UShort = UShort(0xFE9F)
    }

    /**
     * LCDC: LCD control
     *
     * @see [[https://gbdev.io/pandocs/LCDC.html#ff40--lcdc-lcd-control]]
     */
    val LCDC: UShort = UShort(0xFF40)

    /**
     * STAT: LCD status
     *
     * @see [[https://gbdev.io/pandocs/STAT.html#ff41--stat-lcd-status]]
     */
    val STAT: UShort = UShort(0xFF41)

    /**
     * SCY: Background viewport Y position
     *
     * @see [[https://gbdev.io/pandocs/Scrolling.html#ff42ff43--scy-scx-background-viewport-y-position-x-position]]
     */
    val SCY: UShort = UShort(0xFF42)

    /**
     * SCX: Background viewport X position
     *
     * @see [[https://gbdev.io/pandocs/Scrolling.html#ff42ff43--scy-scx-background-viewport-y-position-x-position]]
     */
    val SCX: UShort = UShort(0xFF43)

    /**
     * LY: LCD Y coordinate [read-only]
     *
     * @see [[https://gbdev.io/pandocs/STAT.html#ff44--ly-lcd-y-coordinate-read-only]]
     */
    val LY: UShort = UShort(0xFF44)

    /**
     * LYC: LY compare
     *
     * @see [[https://gbdev.io/pandocs/STAT.html#ff45--lyc-ly-compare]]
     */
    val LYC: UShort = UShort(0xFF45)

    /**
     * DMA: OAM DMA source address & start
     *
     * @see [[https://gbdev.io/pandocs/OAM_DMA_Transfer.html#ff46--dma-oam-dma-source-address--start]]
     */
    val DMA: UShort = UShort(0xFF46)

    /**
     * BGP (Non-CGB Mode only): BG palette data
     *
     * @see [[https://gbdev.io/pandocs/Palettes.html#ff47--bgp-non-cgb-mode-only-bg-palette-data]]
     */
    val BGP: UShort = UShort(0xFF47)

    /**
     * OBP0 (Non-CGB Mode only): OBJ palette 0 data
     *
     * @see [[https://gbdev.io/pandocs/Palettes.html#ff48ff49--obp0-obp1-non-cgb-mode-only-obj-palette-0-1-data]]
     */
    val OBP0: UShort = UShort(0xFF48)

    /**
     * OBP1 (Non-CGB Mode only): OBJ palette 1 data
     *
     * @see [[https://gbdev.io/pandocs/Palettes.html#ff48ff49--obp0-obp1-non-cgb-mode-only-obj-palette-0-1-data]]
     */
    val OBP1: UShort = UShort(0xFF49)

    /**
     * WY: Window Y position
     *
     * @see [[https://gbdev.io/pandocs/Scrolling.html#ff4aff4b--wy-wx-window-y-position-x-position-plus-7]]
     */
    val WY: UShort = UShort(0xFF4A)

    /**
     * WX: Window X position plus 7
     *
     * @see [[https://gbdev.io/pandocs/Scrolling.html#ff4aff4b--wy-wx-window-y-position-x-position-plus-7]]
     */
    val WX: UShort = UShort(0xFF4B)
  }

  val VRAM_SIZE: Int = (Address.VRAM.END - Address.VRAM.START + 1.toUShort).toInt
  val OAM_SIZE: Int = (Address.OAM.END - Address.OAM.START + 1.toUShort).toInt

}
