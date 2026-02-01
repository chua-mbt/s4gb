package org.akaii.s4gb.emulator

import spire.math.{UByte, UShort}

package object byteops {
  extension (value: UShort) {
    def toHexString: String = f"0x${value.toInt}%04X"
    def registerHiByte: UByte = UByte((value.toInt >>> 8) & 0xFF)
    def registerLoByte: UByte = UByte(value.toInt & 0xFF)

    def overflowFromBit11(other: UShort): Boolean =
      ((value.toInt & 0x0FFF) + (other.toInt & 0x0FFF)) > 0x0FFF
  }

  extension (value: UByte) {
    def toHexString: String = f"0x${value.toInt}%02X"
    def toUShort: UShort = UShort(value.toInt & 0xFFFF)

    def overflowFromBit3(other: UByte, carry: UByte = 0.toUByte): Boolean =
      ((value.toInt & 0x0F) + (other.toInt & 0x0F) + carry.toInt) > 0x0F

    def overflowFromBit7(other: UByte, carry: UByte = 0.toUByte): Boolean =
      (value.toInt + other.toInt + carry.toInt) > 0xFF

    def borrowFromBit4(other: UByte, carry: UByte = 0.toUByte): Boolean =
      (value.toInt & 0x0F) < ((other.toInt & 0x0F) + carry.toInt)

    def borrowFrom(other: UByte, carry: UByte = 0.toUByte): Boolean =
      value.toInt < (other.toInt + carry.toInt)
  }

  extension(value: Int) {
    def toUByte: UByte = UByte(value & 0xFF)
    def toUShort: UShort = UShort(value & 0xFFFF)

    def toInstructionInput: Array[UByte] = Array(
      UByte(value & 0xFF),
      UByte((value >>> 8) & 0xFF),
      UByte((value >>> 16) & 0xFF)
    )
  }
}
