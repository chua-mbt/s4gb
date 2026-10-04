package org.akaii.s4gb.emulator.components.ppu

import munit.FunSuite
import org.akaii.s4gb.emulator.components.Interrupts
import org.akaii.s4gb.emulator.components.ppu.fetcher.PixelFetcher
import org.akaii.s4gb.extensions.byteops.*
import spire.math.UByte

class PpuModeTests extends FunSuite with TestEmitters {

  import PpuModeTests.*

  test("OamScan transitions to Draw after 80 dots and applies the mode 2 CPU access rules") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts, nullEmitter)
    ppu.initialize()

    ppu.state.registers(Ppu.Address.LCDC) = UByte(0x00)
    ppu.state.ly = 0.toUByte
    ppu.state.lcdStatus.ppuMode = PpuMode.OamScan
    ppu.state.scanlineDot.current = 0
    ppu.state.objectFetcher.scanlineObjectIndex = 5

    // VRAM is accessible in mode 2, OAM is blocked: reads return garbage and writes are dropped
    ppu.write(Ppu.Address.VRAM.START, UByte(0x42))
    assertEquals(ppu(Ppu.Address.VRAM.START), UByte(0x42))

    ppu.write(Ppu.Address.OAM.START, UByte(0x42))
    assertEquals(ppu(Ppu.Address.OAM.START), Ppu.GARBAGE)

    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank
    assertEquals(ppu(Ppu.Address.OAM.START), UByte(0))
    ppu.state.lcdStatus.ppuMode = PpuMode.OamScan

    ticksUntilTransition(
      ppu,
      expectedDots = PpuMode.OamScan.TOTAL_DOTS,
      expectedFromMode = PpuMode.OamScan,
      expectedToMode = PpuMode.Draw
    )
    assertEquals(ppu.state.objectFetcher.scanlineObjectIndex, 0) // beginMode3 rewinds the object queue

    // Both are blocked in mode 3
    assertEquals(ppu(Ppu.Address.VRAM.START), Ppu.GARBAGE)
    assertEquals(ppu(Ppu.Address.OAM.START), Ppu.GARBAGE)

    ppu.write(Ppu.Address.VRAM.START, UByte(0x99))
    ppu.write(Ppu.Address.OAM.START, UByte(0x99))
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank
    assertEquals(ppu(Ppu.Address.VRAM.START), UByte(0x42))
    assertEquals(ppu(Ppu.Address.OAM.START), UByte(0))
  }

  test("OamScan reads object zero on the dot that starts it") {
    val oam = objectInOam(index = 0, x = OBJECT_LEFT_EDGE + 8, tileIndex = OBJECT_TILE_A)
    val ppu = scanlinePpu(RecordingEmitter(), checkerboardVram(), oam = oam, objEnable = true)

    ticksUntilTransition(ppu, 1, PpuMode.HorizontalBlank, PpuMode.OamScan)

    assertEquals(ppu.state.scanlineObjects.head.used, true)
    assertEquals(ppu.state.scanlineObjects.head.tileIndex.toInt, OBJECT_TILE_A)
  }

  test("OamScan empties the scanline object buffer before the next scan") {
    val oam = objectInOam(index = 0, x = OBJECT_LEFT_EDGE + 8, tileIndex = OBJECT_TILE_A)
    val ppu = scanlinePpu(RecordingEmitter(), checkerboardVram(), oam = oam, objEnable = true)
    renderScanline(ppu)
    assertEquals(ppu.state.scanlineObjects.head.used, true)

    ppu.state.ly = 20.toUByte // the object only reaches screen rows 0 to 7
    ticksWhileIn(ppu, PpuMode.HorizontalBlank) // runs out the line to the dot that starts the next scan

    assertEquals(ppu.state.scanlineObjects.head.used, false)
  }

  test("Draw completes a background scanline in the minimum time and ends in HBlank") {
    val interrupts = Interrupts()
    val emitter = RecordingEmitter()
    val ppu = scanlinePpu(emitter, checkerboardVram(), interrupts = interrupts)
    ppu.state.lcdStatus.mode0Select = true

    startScanline(ppu)
    val drawDots = ticksWhileIn(ppu, PpuMode.Draw)

    assertEquals(drawDots, MINIMUM_DRAW_DOTS)
    assertEquals(emitter.totalEmitted, Ppu.VISIBLE_WIDTH)
    assertEquals(ppu.state.pixelMixer.renderedPixels, Ppu.VISIBLE_WIDTH)
    assertEquals(emitter.row(DRAWN_ROW), expectedCheckerboard)
    assertEquals(ppu.state.lcdStatus.ppuMode, PpuMode.HorizontalBlank)
    assertInterrupts(interrupts, Interrupts.Source.LCDStat)
  }

  test("Draw costs an object fetch more dots than background alone") {
    val backgroundOnly = drawDotsWithObjectAt(OBJECT_LEFT_EDGE + 9, withObject = false)

    val fetchLength = OBJECT_FETCH_DOTS

    // The shifter resumes the moment the fetch ends and idles until the background fetcher
    // has refilled, so what the fetch left in the FIFO decides the whole penalty:
    // "The delay applied here is equal to 6 - REMAINING_PIXEL_COUNT".
    // @see https://github.com/Ashiepaws/GBEDG/blob/master/ppu/index.md#timing-oddities
    assertEquals(drawDotsWithObjectAt(OBJECT_LEFT_EDGE + 9, withObject = true) - backgroundOnly, fetchLength + 1)
    assertEquals(drawDotsWithObjectAt(OBJECT_LEFT_EDGE + 8, withObject = true) - backgroundOnly, fetchLength + fetchLength)
  }

  test("Draw costs the fine scroll one dot per pixel dropped") {
    List(1, 3, 7).foreach { fineScroll =>
      val ppu = scanlinePpu(RecordingEmitter(), checkerboardVram(), scx = fineScroll)

      startScanline(ppu)

      // The mixer drops the first SCX % 8 pixels, so the fetcher has to produce that many extra
      assertEquals(ticksWhileIn(ppu, PpuMode.Draw), MINIMUM_DRAW_DOTS + fineScroll)
    }
  }

  test("Draw arbitrates between the background and object fetchers") {
    val oam = Array.fill(Ppu.OAM_SIZE)(UByte(0))
    writeOamObject(oam, index = 0, x = OBJECT_LEFT_EDGE + 8, tileIndex = OBJECT_TILE_A)
    writeOamObject(oam, index = 1, x = OBJECT_LEFT_EDGE + 40, tileIndex = OBJECT_TILE_B)
    val vram = checkerboardVram()
    fillTile(vram, OBJECT_TILE_A, low = 0xFF, high = 0xF0)
    fillTile(vram, OBJECT_TILE_B, low = 0xFF, high = 0x0F)
    val emitter = RecordingEmitter()
    val ppu = scanlinePpu(emitter, vram, oam = oam, objEnable = true)

    startScanline(ppu)

    // Two objects on the line, so every branch of the fetcher arbitration is taken: the
    // background has the dot, a fetch is started, and the fetch owns the dots until it ends.
    var dots = 0
    var fetchesStarted = 0
    var fetchesOwnedTheDot = 0
    var emittedDuringFetch = 0
    while (ppu.state.lcdStatus.ppuMode == PpuMode.Draw && dots < MAX_MODE_DOTS) {
      val fetchingBefore = ppu.state.objectFetcher.isFetching
      val emittedBefore = emitter.totalEmitted
      ppu.tick()
      if (!fetchingBefore && ppu.state.objectFetcher.isFetching) fetchesStarted += 1
      if (fetchingBefore) {
        fetchesOwnedTheDot += 1
        if (emitter.totalEmitted != emittedBefore) emittedDuringFetch += 1
      }
      dots += 1
    }
    assert(ppu.state.lcdStatus.ppuMode != PpuMode.Draw, s"still in Draw after $MAX_MODE_DOTS dots")

    assertEquals(fetchesStarted, 2)
    assertEquals(ppu.state.objectFetcher.scanlineObjectIndex, 2) // one cursor step per fetch
    assertEquals(fetchesOwnedTheDot, 2 * OBJECT_FETCH_DOTS) // the dot a fetch starts on is not its own
    assertEquals(emittedDuringFetch, 0)
    assertEquals(emitter.totalEmitted, Ppu.VISIBLE_WIDTH)
  }

  test("Draw fetches no object while LCDC.1 (object display enable) is clear") {
    val emitter = RecordingEmitter()
    val vram = checkerboardVram()
    fillTile(vram, OBJECT_TILE_A, low = 0xFF, high = 0xFF)
    val oam = objectInOam(index = 0, x = OBJECT_LEFT_EDGE + 8, tileIndex = OBJECT_TILE_A)
    val ppu = scanlinePpu(emitter, vram, oam = oam)

    renderScanline(ppu)

    assertEquals(emitter.row(DRAWN_ROW).map(_.toInt), expectedCheckerboard.map(_.toInt))
    assertEquals(ppu.state.objectFetcher.scanlineObjectIndex, 0)
  }

  test("Draw cancels a fetch in progress when objects are disabled") {
    val state = Ppu.State(
      emitter = nullEmitter,
      vram = Array.fill(Ppu.VRAM_SIZE)(UByte(0)),
      oam = Array.fill(Ppu.OAM_SIZE)(UByte(0)),
      lcdControl = LcdControl(objEnable = true),
    )
    state.scanlineObjects(0).set(y = 16.toUByte, x = 8.toUByte, tileIndex = 1.toUByte, attributes = 0.toUByte)
    val interrupts = Interrupts()

    PpuMode.Draw.tick(state, interrupts)
    assertEquals(state.objectFetcher.isFetching, true)

    state.lcdControl.objEnable = false
    PpuMode.Draw.tick(state, interrupts)

    assertEquals(state.objectFetcher.isFetching, false)
    assertEquals(state.objectFifo.size, 0)
  }

  test("Draw renders an object row from its left edge over the background") {
    val emitter = RecordingEmitter()
    val vram = checkerboardVram()
    fillTile(vram, OBJECT_TILE_A, low = 0xFF, high = 0xF0) // 3,3,3,3,1,1,1,1
    val oam = objectInOam(index = 0, x = OBJECT_LEFT_EDGE + 8, tileIndex = OBJECT_TILE_A)
    val ppu = scanlinePpu(emitter, vram, oam = oam, objEnable = true)

    renderScanline(ppu)

    val drawn = emitter.row(DRAWN_ROW).map(_.toInt)
    assertEquals(drawn.slice(OBJECT_LEFT_EDGE, OBJECT_LEFT_EDGE + Tile.SIZE), Seq(3, 3, 3, 3, 1, 1, 1, 1))
    assertEquals(drawn.take(OBJECT_LEFT_EDGE), expectedCheckerboard.take(OBJECT_LEFT_EDGE).map(_.toInt))
  }

  test("Draw hands the background over to the window from WX - 7") {
    val emitter = RecordingEmitter()
    val vram = checkerboardVram(windowTile = OBJECT_TILE_A)
    val ppu = scanlinePpu(emitter, vram, wx = OBJECT_LEFT_EDGE + 7, windowEnable = true, windowTileMap = true)

    renderScanline(ppu)

    val drawn = emitter.row(DRAWN_ROW).map(_.toInt)
    assertEquals(drawn.take(OBJECT_LEFT_EDGE), expectedCheckerboard.take(OBJECT_LEFT_EDGE).map(_.toInt))
    assertEquals(drawn.slice(OBJECT_LEFT_EDGE, OBJECT_LEFT_EDGE + Tile.SIZE), Seq(3, 1, 3, 1, 3, 1, 3, 1))
  }

  test("HorizontalBlank transitions to OamScan at the end of a visible scanline") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts, nullEmitter)
    ppu.initialize()

    ppu.state.ly = 0.toUByte
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank
    ppu.state.lcdStatus.mode2Select = true
    ppu.state.scanlineDot.current = 0

    ticksUntilTransition(
      ppu,
      expectedDots = ScanlineDot.DOTS_PER_LINE,
      expectedFromMode = PpuMode.HorizontalBlank,
      expectedToMode = PpuMode.OamScan
    )

    assertEquals(ppu.state.ly, 1.toUByte)
    assertInterrupts(interrupts, Interrupts.Source.LCDStat)
  }

  test("HorizontalBlank transitions to VerticalBlank on the last visible scanline") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts, nullEmitter)
    ppu.initialize()

    ppu.state.ly = PpuMode.VISIBLE_SCANLINES_END
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank
    ppu.state.lcdStatus.mode1Select = true
    ppu.state.scanlineDot.current = 0
    ppu.state.backgroundFetcher.windowRowsRendered = 5

    ticksUntilTransition(
      ppu,
      expectedDots = ScanlineDot.DOTS_PER_LINE,
      expectedFromMode = PpuMode.HorizontalBlank,
      expectedToMode = PpuMode.VerticalBlank
    )

    assertEquals(ppu.state.ly, PpuMode.VBLANK_START_LY)
    assertInterrupts(interrupts, Interrupts.Source.VBlank, Interrupts.Source.LCDStat)
    assertEquals(ppu.state.backgroundFetcher.windowRowsRendered, 0) // reset on VBlank entry
  }

  test("VerticalBlank persists for all 10 scanlines before transitioning to OamScan") {
    val interrupts = Interrupts()
    val ppu = Ppu(interrupts, nullEmitter)
    ppu.initialize()

    ppu.state.ly = PpuMode.VBLANK_START_LY
    ppu.state.lcdStatus.ppuMode = PpuMode.VerticalBlank
    ppu.state.lcdStatus.mode2Select = true
    ppu.state.scanlineDot.current = 0

    val scanlinesUntilTransition = Ppu.TOTAL_VBLANK_SCANLINES - 1 // 9
    val dotsUntilFinalScanline = scanlinesUntilTransition * ScanlineDot.DOTS_PER_LINE
    var dotsElapsed = 0
    while (dotsElapsed < dotsUntilFinalScanline) {
      ppu.tick()
      dotsElapsed += 1
      assertEquals(ppu.state.lcdStatus.ppuMode, PpuMode.VerticalBlank)
    }
    assertEquals(ppu.state.ly, PpuMode.VBLANK_START_LY + scanlinesUntilTransition.toUByte)

    ticksUntilTransition(
      ppu,
      expectedDots = ScanlineDot.DOTS_PER_LINE,
      expectedFromMode = PpuMode.VerticalBlank,
      expectedToMode = PpuMode.OamScan
    )

    assertEquals(ppu.state.ly, PpuMode.FRAME_WRAP_LY)
    assertInterrupts(interrupts, Interrupts.Source.LCDStat)
  }
}

object PpuModeTests {

  /** Tiles 0 to 19 hold the background, tiles 30 and 31 are left for objects. */
  private val OBJECT_TILE_A: Int = 30
  private val OBJECT_TILE_B: Int = 31
  private val OBJECT_LEFT_EDGE: Int = 16

  /** The boundary tick advances LY before mode 2 reads it, so the scanline drawn is 1. */
  private val DRAWN_ROW: Int = 1

  /**
   * Dots mode 3 needs to emit the 160 visible background pixels with the fine scroll at zero.
   * Each pixel dropped by the fine scroll costs one more dot, since the fetcher has to replace it.
   */
  private val MINIMUM_DRAW_DOTS: Int = 166

  /** Ceiling on how long any one mode may last, so a mode that never exits fails rather than hangs. */
  private val MAX_MODE_DOTS: Int = 2 * ScanlineDot.DOTS_PER_LINE

  /**
   * Dots the object fetcher owns in one fetch, during which the background fetcher is paused
   * and no pixel is emitted. Three of its four steps run for [[PixelFetcher.TWO_DOT_MAX]] dots,
   * GetTile, GetTileDataLow and GetTileDataHigh, and Push is a single dot.
   */
  private val OBJECT_FETCH_DOTS: Int = 3 * PixelFetcher.TWO_DOT_MAX + 1

  /** Visible area is 20 tiles wide, with the scroll registers at zero. */
  private val TILES_PER_LINE: Int = Ppu.VISIBLE_WIDTH / Tile.SIZE

  /** Alternating 8 pixel runs of colour 1 and colour 0, so a run boundary is visible. */
  private val expectedCheckerboard: Seq[UByte] =
    (0 until TILES_PER_LINE).flatMap(tile => tileRow(0xFF, if (tile % 2 == 0) 0xFF else 0x00))

  /**
   * VRAM holding the checkerboard in the background tile map, and `windowTile` laid
   * across the window tile map so that a test can tell the two layers apart.
   */
  private def checkerboardVram(windowTile: Int = -1): Array[UByte] = {
    val vram = Array.fill(Ppu.VRAM_SIZE)(UByte(0))

    (0 until TILES_PER_LINE).foreach { x =>
      vram(Tile.PRIMARY_TILEMAP_ADDRESS + x) = UByte(x)
      fillTile(vram, x, low = 0xFF, high = if (x % 2 == 0) 0xFF else 0x00)
    }

    if (windowTile >= 0) {
      (0 until TILES_PER_LINE).foreach(x => vram(Tile.SECONDARY_TILEMAP_ADDRESS + x) = UByte(windowTile))
      fillTile(vram, windowTile, low = 0xFF, high = 0xAA)
    }

    vram
  }

  /** The eight colour indices of a tile row, the high bit of each byte leftmost. */
  private def tileRow(low: Int, high: Int): Seq[UByte] =
    (0 until Tile.SIZE).map { column =>
      val bit = Tile.SIZE - 1 - column
      val colorIndex = ((high >> bit) & 1) << 1 | ((low >> bit) & 1)
      colorIndex.toUByte
    }

  /** Every row of the tile, so the pattern holds whichever row of it LY lands on. */
  private def fillTile(vram: Array[UByte], tile: Int, low: Int, high: Int): Unit =
    (0 until Tile.SIZE).foreach { row =>
      val offset = tile * Tile.SIZE * 2 + row * 2
      vram(offset) = UByte(low)
      vram(offset + 1) = UByte(high)
    }

  private def writeOamObject(oam: Array[UByte], index: Int, x: Int, tileIndex: Int, attributes: Int = 0): Unit = {
    val offset = index * GameboyObject.BYTE_SIZE
    oam(offset) = UByte(16) // top edge on screen row 0
    oam(offset + 1) = UByte(x)
    oam(offset + 2) = UByte(tileIndex)
    oam(offset + 3) = UByte(attributes)
  }

  private def objectInOam(index: Int, x: Int, tileIndex: Int): Array[UByte] = {
    val oam = Array.fill(Ppu.OAM_SIZE)(UByte(0))
    writeOamObject(oam, index, x, tileIndex)
    oam
  }

  /** A PPU part way into scanline 0, in mode 2 with the registers at their power up values. */
  private def scanlinePpu(
    emitter: PixelEmitter,
    vram: Array[UByte],
    oam: Array[UByte] = Array.fill(Ppu.OAM_SIZE)(UByte(0)),
    interrupts: Interrupts = Interrupts(),
    scx: Int = 0,
    wx: Int = 7,
    objEnable: Boolean = false,
    windowEnable: Boolean = false,
    windowTileMap: Boolean = false
  ): Ppu = {
    val ppu = Ppu(interrupts, emitter, vram, oam)
    ppu.initialize()
    ppu.state.ly = 0.toUByte
    ppu.state.lcdStatus.ppuMode = PpuMode.HorizontalBlank
    // one tick short of the boundary, so the scanline runs the way it does in hardware
    ppu.state.scanlineDot.current = ScanlineDot.DOTS_PER_LINE - 1

    val lcdc = 0x80 | 0x01 | 0x10 | // LCD on, background on, $8000 tile data addressing
      (if (objEnable) 0x02 else 0) |
      (if (windowEnable) 0x20 else 0) |
      (if (windowTileMap) 0x40 else 0)
    ppu.write(Ppu.Address.LCDC, UByte(lcdc))
    ppu.write(Ppu.Address.SCX, UByte(scx))
    ppu.write(Ppu.Address.WX, UByte(wx))
    // identity palettes, so that the emitted colour is the index the tile data held
    ppu.write(Ppu.Address.BGP, UByte(0xE4))
    ppu.write(Ppu.Address.OBP0, UByte(0xE4))

    ppu
  }

  /** Runs one scanline from its boundary through mode 3, returning the dots mode 3 took. */
  private def renderScanline(ppu: Ppu): Int = {
    startScanline(ppu)
    ticksWhileIn(ppu, PpuMode.Draw)
  }

  /** Runs the dot that starts mode 2 and the 80 dots of mode 2 itself. */
  private def startScanline(ppu: Ppu): Unit = {
    ticksUntilTransition(ppu, 1, PpuMode.HorizontalBlank, PpuMode.OamScan)
    ticksUntilTransition(ppu, PpuMode.OamScan.TOTAL_DOTS, PpuMode.OamScan, PpuMode.Draw)
  }

  private def renderRemainderOfScanline(ppu: Ppu): Unit =
    ticksWhileIn(ppu, PpuMode.Draw)

  private def drawDotsWithObjectAt(x: Int, withObject: Boolean): Int = {
    val vram = checkerboardVram()
    fillTile(vram, OBJECT_TILE_A, low = 0xFF, high = 0xFF)
    val oam = if (withObject) objectInOam(0, x, OBJECT_TILE_A) else Array.fill(Ppu.OAM_SIZE)(UByte(0))
    renderScanline(scanlinePpu(RecordingEmitter(), vram, oam = oam, objEnable = withObject))
  }

  def assertInterrupts(interrupts: Interrupts, expected: Interrupts.Source*): Unit = {
    val ifReg = interrupts(Interrupts.Address.INTERRUPT_FLAG)
    Interrupts.Source.values.foreach { source =>
      val mask = UByte(1 << source.bit)
      if (expected.contains(source)) {
        assert((ifReg & mask) != 0.toUByte, s"$source interrupt should be set")
      } else {
        assert((ifReg & mask) == 0.toUByte, s"$source interrupt should not be set")
      }
    }
  }

  /**
   * Ticks the PPU until the mode transitions from expectedFromMode, verifying total execution duration.
   */
  def ticksUntilTransition(ppu: Ppu, expectedDots: Int, expectedFromMode: PpuMode, expectedToMode: PpuMode): PpuMode = {
    assert(ppu.state.lcdStatus.ppuMode == expectedFromMode)
    val dotsElapsed = ticksWhileIn(ppu, expectedFromMode)

    assert(dotsElapsed == expectedDots)
    assert(ppu.state.lcdStatus.ppuMode == expectedToMode)
    ppu.state.lcdStatus.ppuMode
  }

  /** Ticks until `condition` holds, returning the dots it took. Fails if it does not within `maxDots`. */
  private def tickUntil(ppu: Ppu, condition: => Boolean, maxDots: Int): Int = {
    var dotsElapsed = 0
    while (!condition && dotsElapsed < maxDots) {
      ppu.tick()
      dotsElapsed += 1
    }
    assert(condition, s"condition still unmet after $maxDots dots")
    dotsElapsed
  }

  /** Ticks while the PPU is in `mode`, returning the dots it spent there. Bounded by [[MAX_MODE_DOTS]]. */
  def ticksWhileIn(ppu: Ppu, mode: PpuMode): Int = {
    var dotsElapsed = 0
    while (ppu.state.lcdStatus.ppuMode == mode && dotsElapsed < MAX_MODE_DOTS) {
      ppu.tick()
      dotsElapsed += 1
    }
    assert(
      ppu.state.lcdStatus.ppuMode != mode,
      s"still in $mode after $MAX_MODE_DOTS dots, the mode is not exiting"
    )
    dotsElapsed
  }
}
