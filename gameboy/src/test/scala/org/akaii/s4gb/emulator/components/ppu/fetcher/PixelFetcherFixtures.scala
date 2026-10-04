package org.akaii.s4gb.emulator.components.ppu.fetcher

import org.akaii.s4gb.emulator.components.ppu.*
import spire.math.{UByte, UShort}

import scala.collection.mutable

trait PixelFetcherFixtures extends TestEmitters {

  def makeState(
    scx: Int = 0,
    scy: Int = 0,
    ly: Int = 0,
    wx: Int = 7,
    wy: Int = 0,
    bgEnable: Boolean = true,
    bgWindowTileData: Boolean = true,
    bgTileMap: Boolean = false,
    windowTileMap: Boolean = false,
    windowEnable: Boolean = false,
    objSize: Boolean = false,
    vram: Array[UByte] = Array.fill(Ppu.VRAM_SIZE)(UByte(0))
  ): Ppu.State = {
    val registers = mutable.Map[UShort, UByte](
      Ppu.Address.SCX -> UByte(scx),
      Ppu.Address.SCY -> UByte(scy),
      Ppu.Address.WX -> UByte(wx),
      Ppu.Address.WY -> UByte(wy),
    )
    val lcdc = LcdControl(
      bgEnable = bgEnable,
      bgWindowTileData = bgWindowTileData,
      bgTileMap = bgTileMap,
      windowTileMap = windowTileMap,
      windowEnable = windowEnable,
      objSize = objSize,
    )
    val state = Ppu.State(
      emitter = nullEmitter,
      vram = vram,
      registers = registers,
      lcdControl = lcdc,
    )
    state.ly = UByte(ly)
    state
  }

  def tickUntilStepChange(state: Ppu.State, fetcher: PixelFetcher.State, maxTicks: Int): (PixelFetcher.Step, Int) = {
    var ticks = 0
    val initialStep = fetcher.step
    while (fetcher.step == initialStep && ticks < maxTicks) {
      fetcher.step.tick(state)
      ticks += 1
    }
    (fetcher.step, ticks)
  }

  case class CycleData(low: UByte, high: UByte, expectedColors: Seq[Int])
}
