package org.akaii.s4gb.emulator

import spire.math.{UByte, UShort}

package object byteops {
  implicit class UShortOps(val value: UShort) {
    def toHexString: String = f"0x${value.toInt}%04X"
    def registerHiByte: UByte = UByte((value.toInt >>> 8) & 0xFF)
    def registerLoByte: UByte = UByte(value.toInt & 0xFF)
  }

  implicit class UByteOps(val value: UByte) {
    def toHexString: String = f"0x${value.toInt}%02X"
    def toUShort: UShort = UShort(value.toInt & 0xFFFF)
  }

  implicit class IntOps(val value: Int) {
    def toUByte: UByte = UByte(value & 0xFF)
    def toUShort: UShort = UShort(value & 0xFFFF)

    def toInstructionInput: Array[UByte] = Array(
      UByte(value & 0xFF),
      UByte((value >>> 8) & 0xFF),
      UByte((value >>> 16) & 0xFF)
    )
  }
}
