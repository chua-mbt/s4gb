package org.akaii.s4gb.emulator

import spire.math.{UByte, UShort}

package object byteops {
  extension (value: UShort) {
    @inline def toHexString: String = f"0x${value.toInt}%04X"
    @inline def hiByte: UByte = UByte((value.toInt >>> 8) & 0xFF)
    @inline def loByte: UByte = UByte(value.toInt & 0xFF)

    @inline def overflowFromBit11(other: UShort): Boolean =
      ((value.toInt & 0x0FFF) + (other.toInt & 0x0FFF)) > 0x0FFF
  }

  extension (value: UByte) {
    @inline def toHexString: String = f"0x${value.toInt}%02X"
    @inline def toUShort: UShort = UShort(value.toInt & 0xFFFF)

    @inline def overflowFromBit3(other: UByte, carry: UByte = 0.toUByte): Boolean =
      ((value.toInt & 0x0F) + (other.toInt & 0x0F) + carry.toInt) > 0x0F

    @inline def overflowFromBit7(other: UByte, carry: UByte = 0.toUByte): Boolean =
      (value.toInt + other.toInt + carry.toInt) > 0xFF

    @inline def borrowFromBit4(other: UByte, carry: UByte = 0.toUByte): Boolean =
      (value.toInt & 0x0F) < ((other.toInt & 0x0F) + carry.toInt)

    @inline def borrowFrom(other: UByte, carry: UByte = 0.toUByte): Boolean =
      value.toInt < (other.toInt + carry.toInt)
  }

  extension(value: Int) {
    @inline def toUByte: UByte = UByte(value & 0xFF)
    @inline def toUShort: UShort = UShort(value & 0xFFFF)

    @inline def toInstructionInput: Array[UByte] = Array(
      UByte(value & 0xFF),
      UByte((value >>> 8) & 0xFF),
      UByte((value >>> 16) & 0xFF)
    )
  }
}
