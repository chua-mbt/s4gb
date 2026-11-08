package org.akaii.s4gb.emulator.memorymap

import org.akaii.s4gb.extensions.byteops.*
import spire.math.{UByte, UShort}

/**
 * Array-backed storage for a contiguous address range.
 */
abstract class RegisterMap(val start: UShort, val end: UShort) extends MemoryMap {

  require(start.toInt <= end.toInt, f"Invalid register range: 0x${start.toInt}%04X-0x${end.toInt}%04X")

  protected val size: Int = end.toInt - start.toInt + 1
  private val storage: Array[UByte] = Array.fill[UByte](size)(UByte(0))

  protected def contains(address: UShort): Boolean = start <= address && address <= end

  protected def stored(address: UShort): UByte = storage(indexOf(address))

  protected def store(address: UShort, value: UByte): Unit = storage(indexOf(address)) = value

  private def indexOf(address: UShort): Int = address.toInt - start.toInt

  def apply(address: UShort): UByte =
    if (contains(address)) stored(address)
    else throw new IllegalArgumentException(s"Invalid register address: $address")

  def write(address: UShort, value: UByte): Unit =
    if (contains(address)) store(address, value)
    else throw new IllegalArgumentException(s"Invalid register address: $address")

  def update(address: UShort, value: UByte): Unit = write(address, value)

  override def toString: String = {
    val entries = (0 until size).iterator
      .map(index => (start.toInt + index).toUShort -> storage(index))
      .filter(_._2 != UByte(0))
      .map { case (address, value) => f"0x${address.toInt}%04X=0x${value.toInt}%02X" }
      .mkString(",")
    s"${getClass.getSimpleName}($entries)"
  }
}
