package org.akaii.s4gb.blargg

import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

class BlarggTestMemoryMap extends MemoryMap {
  // TODO: Use Serial I/O constants instead of hardcoded addresses (0xFF01, 0xFF02)
  private val data = Array.fill[UByte](0x10000)(UByte(0))
  private val serialBuffer = new StringBuilder()
  private var serialControl: UByte = UByte(0)

  override def apply(address: UShort): UByte = address.toInt match {
    case 0xFF01 => data(address.toInt)
    case 0xFF02 => serialControl
    case 0xFF44 => UByte(0x90) // LY register - return 90 for gameboy-doctor compatibility
    case _ => data(address.toInt)
  }

  override def write(address: UShort, value: UByte): Unit = address.toInt match {
    case 0xFF01 =>
      data(address.toInt) = value
      if ((serialControl.toInt & 0x80) != 0) {
        serialBuffer.append(value.toChar)
      }
    case 0xFF02 =>
      serialControl = value
    case _ =>
      data(address.toInt) = value
  }

  override def fetchIfPresent(address: UShort): Option[UByte] = Some(apply(address))

  def serialOutput: String = serialBuffer.toString
}
