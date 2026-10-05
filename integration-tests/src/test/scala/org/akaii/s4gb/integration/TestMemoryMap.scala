package org.akaii.s4gb.integration

import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

/** Work RAM plus serial output capture. Suites report their result over the serial port. */
class TestMemoryMap extends MemoryMap {

  private val data = Array.fill[UByte](MemoryMap.MEMORY_SIZE)(UByte(0))
  private val serialBuffer = collection.mutable.ArrayBuffer.empty[Byte]
  private var serialControl: UByte = UByte(0)


  override def apply(address: UShort): UByte = address.toInt match {
    case TestMemoryMap.SB => data(address.toInt)
    case TestMemoryMap.SC => serialControl
    case _ => data(address.toInt)
  }

  override def write(address: UShort, value: UByte): Unit = address.toInt match {
    case TestMemoryMap.SB =>
      data(address.toInt) = value
      // Writing SB while a transfer is already armed starts one immediately.
      if (transferArmed) transmit(value)
    case TestMemoryMap.SC =>
      // Writing SC with the start bit going up starts a transfer of whatever SB holds.
      val starts = !transferArmed && (value.toInt & TestMemoryMap.TRANSFER_START) != 0
      serialControl = value
      if (starts) transmit(data(TestMemoryMap.SB))
    case _ =>
      data(address.toInt) = value
  }

  override def fetchIfPresent(address: UShort): Option[UByte] = Some(apply(address))

  def serialOutput: String = serialBuffer.map(_.toChar).mkString

  /** Raw bytes off the link port, which is how Mooneye signals pass/fail. */
  def serialBytes: Seq[Byte] = serialBuffer.toSeq

  /** Hardware clears the start bit once the byte has been shifted out. */
  private def transmit(byte: UByte): Unit = {
    serialBuffer += (byte.toInt & 0xFF).toByte
    serialControl = UByte(serialControl.toInt & ~TestMemoryMap.TRANSFER_START)
  }

  private def transferArmed: Boolean = (serialControl.toInt & TestMemoryMap.TRANSFER_START) != 0
}

object TestMemoryMap {
  // TODO: use Serial I/O constants once there is a Serial component.
  private[integration] val SB: Int = 0xFF01
  private[integration] val SC: Int = 0xFF02
  private[integration] val TRANSFER_START: Int = 0x80
}