package org.akaii.s4gb.integration

import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

/** Reads serial output from the bus and clears the start bit to complete each transfer. */
final class SerialCapture(bus: MemoryMap) {

  private val serialBuffer = collection.mutable.ArrayBuffer.empty[Byte]

  def observe(): Unit = {
    val control = bus(SerialCapture.SC).toInt
    if ((control & SerialCapture.TRANSFER_START) != 0) {
      serialBuffer += (bus(SerialCapture.SB).toInt & 0xFF).toByte
      bus.write(SerialCapture.SC, UByte(control & ~SerialCapture.TRANSFER_START))
    }
  }

  def serialOutput: String = serialBuffer.map(_.toChar).mkString

  def serialBytes: Seq[Byte] = serialBuffer.toSeq
}

object SerialCapture {
  private val SB: UShort = UShort(0xFF01)
  private val SC: UShort = UShort(0xFF02)
  private val TRANSFER_START: Int = 0x80
}
