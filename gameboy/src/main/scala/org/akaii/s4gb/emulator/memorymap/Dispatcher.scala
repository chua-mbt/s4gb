package org.akaii.s4gb.emulator.memorymap

import org.akaii.s4gb.emulator.components.*
import spire.math.{UByte, UShort}

/**
 * Dispatches memory reads/writes to the appropriate component based on address range.
 */
class Dispatcher private (components: MemoryMap*) extends MemoryMap {

  override def apply(address: UShort): UByte =
    componentFor(address).apply(address)

  override def write(address: UShort, value: UByte): Unit =
    componentFor(address).write(address, value)

  override def fetchIfPresent(address: UShort): Option[UByte] =
    componentForOption(address).map(_.apply(address))

  private def componentFor(address: UShort): MemoryMap =
    components.find(contains(_, address))
      .getOrElse(throw new IllegalArgumentException(f"No component for address: 0x${address.toInt}%04X"))

  private def componentForOption(address: UShort): Option[MemoryMap] =
    components.find(contains(_, address))

  private def contains(component: MemoryMap, address: UShort): Boolean =
    component match {
      case rc: Dispatcher.RangeComponent =>
        rc.start <= address && address <= rc.end
      case _ =>
        false
    }
}

object Dispatcher {
  def withComponents: Dispatcher = {
    val interrupts = Interrupts()
    val joypad = Joypad(interrupts)
    val rom = Rom(Array.fill(0x8000)(UByte(0)))
    val ppu = Ppu()

    withRanges(
      (Rom.Address.ROM_START -> Rom.Address.ROM_END) -> rom,
      (Ppu.Address.VRAM.START -> Ppu.Address.VRAM.END) -> ppu,
      (Ppu.Address.OAM.START -> Ppu.Address.OAM.END) -> ppu,
      (Ppu.Address.LCDC -> Ppu.Address.WX) -> ppu,
      (Joypad.Address.JOYPAD -> Joypad.Address.JOYPAD) -> joypad,
      (Interrupts.Address.INTERRUPT_FLAG -> Interrupts.Address.INTERRUPT_FLAG) -> interrupts,
      (Interrupts.Address.INTERRUPT_ENABLE -> Interrupts.Address.INTERRUPT_ENABLE) -> interrupts,
    )
  }

  def withRanges(ranges: ((UShort, UShort), MemoryMap)*): Dispatcher =
    new Dispatcher(ranges.map { case ((start, end), component) =>
      new RangeComponent(start, end, component)
    }*)

  private class RangeComponent(
    val start: UShort,
    val end: UShort,
    component: MemoryMap
  ) extends MemoryMap {
    override def apply(address: UShort): UByte = component(address)
    override def write(address: UShort, value: UByte): Unit = component.write(address, value)
    override def fetchIfPresent(address: UShort): Option[UByte] = component.fetchIfPresent(address)
  }
}
