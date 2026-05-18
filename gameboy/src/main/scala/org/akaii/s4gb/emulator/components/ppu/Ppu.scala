package org.akaii.s4gb.emulator.components.ppu

import org.akaii.s4gb.collections.RingBuffer
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.memorymap.RegisterMap
import org.akaii.s4gb.extensions.byteops.*
import spire.math.{UByte, UShort}

import scala.collection.mutable

/**
 * Picture Processing Unit
 *
 * @see [[https://gbdev.io/pandocs/Rendering.html#ppu-modes]]
 */
class Ppu(interrupts: Interrupts, vram: Array[UByte], oam: Array[UByte]) extends RegisterMap {

  import Ppu.*
  import Ppu.Address.*

  /**
   * Initialize hardware registers with their DMG boot values.
   */
  override protected val registers: mutable.Map[UShort, UByte] = mutable.Map(
    SCY -> UByte(0),
    SCX -> UByte(0),
    LYC -> UByte(0),
    BGP -> UByte(0),
    OBP0 -> UByte(0),
    OBP1 -> UByte(0),
    WY -> UByte(0),
    WX -> UByte(0),
  )

  private[ppu] val state: Ppu.State = Ppu.State(
    oam = oam.clone(),
    vram = vram.clone(),
    registers = registers,
  )

  /**
   * Initializes registers to DMG power-up state.
   *
   * @see [[https://gbdev.io/pandocs/Power_Up_Sequence.html#hardware-registers]]
   */
  def initialize(): Unit = {
    registers(SCY) = UByte(0x00)
    registers(SCX) = UByte(0x00)
    registers(LYC) = UByte(0x00)
    registers(BGP) = UByte(0xFC)
    registers(OBP0) = UByte(0xFF)
    registers(OBP1) = UByte(0xFF)
    registers(WY) = UByte(0x00)
    registers(WX) = UByte(0x00)
    state.ly = UByte(0x00)
    state.lcdStatus.initialize()
    state.lcdControl.initialize()
  }

  override def apply(address: UShort): UByte =
    if (address == STAT) {
      state.lcdStatus.read(state.lcdControl.lcdEnable)
    } else if (address == LCDC) {
      state.lcdControl.read()
    } else if (address == LY) {
      state.ly
    } else if (isVram(address)) {
      if (state.lcdStatus.ppuMode.canAccessVram) state.vram(vramIndex(address)) else GARBAGE
    } else if (isOam(address)) {
      if (state.lcdStatus.ppuMode.canAccessOam) state.oam(oamIndex(address)) else GARBAGE
    } else {
      super.apply(address)
    }

  override def write(address: UShort, value: UByte): Unit =
    if (address == STAT) {
      state.lcdStatus.write(value)
    } else if (address == LCDC) {
      state.lcdControl.write(value)
    } else if (address == LYC) {
      registers(LYC) = value
      state.updateLycEqualsLy(interrupts)
    } else if (address == LY) {
      () // LY is read-only
    } else if (isVram(address) && state.lcdStatus.ppuMode.canAccessVram) {
      state.vram(vramIndex(address)) = value
    } else if (isOam(address) && state.lcdStatus.ppuMode.canAccessOam) {
      state.oam(oamIndex(address)) = value
    } else {
      super.write(address, value)
    }

  def tick(): Unit = {
    updateDot()
    if (state.scanlineDot.isBoundary) updateScanline()
    val nextMode = state.lcdStatus.ppuMode.tick(state, interrupts)
    state.lcdStatus.ppuMode = nextMode
  }

  private def updateDot(): Unit = {
    state.scanlineDot.current = state.scanlineDot.current + 1
    state.scanlineDot.cumulative = state.scanlineDot.cumulative + 1
    if (state.scanlineDot.current > ScanlineDot.DOTS_PER_LINE) {
      state.scanlineDot.current = 0
      state.scanlineDot.cumulative = state.scanlineDot.cumulative + 1
    }
  }

  private def updateScanline(): Unit = {
    state.ly = state.ly + 1.toUByte match {
      case ly if ly >= SCANLINES_PER_FRAME => 0.toUByte
      case ly => ly
    }
    state.updateLycEqualsLy(interrupts)
  }

  @inline private def isVram(address: UShort): Boolean = address >= VRAM.START && address <= VRAM.END

  @inline private def isOam(address: UShort): Boolean = address >= OAM.START && address <= OAM.END

  @inline private def vramIndex(address: UShort): Int = address.toInt - VRAM.START.toInt

  @inline private def oamIndex(address: UShort): Int = address.toInt - OAM.START.toInt
}

object Ppu {

  def apply(
    interrupts: Interrupts,
    vram: Array[UByte] = Array.fill(Ppu.VRAM_SIZE)(UByte(0)),
    oam: Array[UByte] = Array.fill(Ppu.OAM_SIZE)(UByte(0))
  ): Ppu = new Ppu(interrupts, vram, oam)

  case class State(
    oam: Array[UByte] = Array.empty,
    vram: Array[UByte] = Array.empty,
    registers: mutable.Map[UShort, UByte] = mutable.Map.empty,
    scanlineObjects: Array[GameboyObject] = Array.empty,
    backgroundFifo: RingBuffer[Pixel] = RingBuffer[Pixel](FIFO_SIZE),
    objectFifo: RingBuffer[Pixel] = RingBuffer[Pixel](FIFO_SIZE),
    scanlineDot: ScanlineDot = ScanlineDot(),
    lcdStatus: LcdStatus = LcdStatus(),
    lcdControl: LcdControl = LcdControl(),
    var ly: UByte = 0.toUByte,
  ) {
    def resetScanlineObjects(): Unit = scanlineObjects.foreach(_.reset())

    def resetFifos(): Unit = {
      backgroundFifo.clear()
      objectFifo.clear()
    }

    def updateLycEqualsLy(interrupts: Interrupts): Unit = {
      val hit = ly == registers(Ppu.Address.LYC)
      if(hit && lcdStatus.lycSelect) interrupts.request(Interrupts.Source.LCDStat)
      lcdStatus.lycEqualsLy = hit
    }
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
  val SCANLINES_PER_FRAME: UByte = UByte(154)
  val FIFO_SIZE: Int = 16


  /**
   * While the PPU is accessing some video-related memory, that memory is inaccessible to the CPU
   * (writes are ignored, and reads return garbage values, usually $FF).
   *
   * @see [[https://gbdev.io/pandocs/Rendering.html#ppu-modes]]
   * @see [[https://gbdev.io/pandocs/Accessing_VRAM_and_OAM.html#accessing-vram-and-oam]]
   */
  val GARBAGE: UByte = UByte(0xFF)
}
