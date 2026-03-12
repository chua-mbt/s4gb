package org.akaii.s4gb.emulator.memorymap

import spire.math.{UByte, UShort}

import scala.collection.mutable

abstract class RegisterMap extends MemoryMap with Product with Serializable {

  protected val registers: mutable.Map[UShort, UByte]

  def apply(address: UShort): UByte =
    registers.getOrElse(
      address,
      throw new IllegalArgumentException(s"Invalid register address: $address")
    )

  def write(address: UShort, value: UByte): Unit =
    if (registers.contains(address))
      registers(address) = value
    else
      throw new IllegalArgumentException(s"Invalid register address: $address")

  override def toString: String = {
    val entries =
      registers
        .toSeq
        .sortBy(_._1.toInt)
        .collect { case (k, v) if v != UByte(0) =>
          f"0x${k.toInt}%04X=0x${v.toInt}%02X"
        }
        .mkString(",")
    s"$productPrefix($entries)"
  }
}