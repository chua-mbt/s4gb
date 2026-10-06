package org.akaii.s4gb.emulator.memorymap

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
