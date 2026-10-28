package org.akaii.s4gb.emulator.memorymap

import munit.FunSuite
import spire.math.{UByte, UShort}

class StorageMapTests extends FunSuite {

  import StorageMapTests.*

  test("reads default to zero at start, middle, and end") {
    val storage = freshStorage()
    assertEquals(storage(StorageMap.Address.HRAM_START), UByte(0))
    assertEquals(storage(MID), UByte(0))
    assertEquals(storage(StorageMap.Address.HRAM_END), UByte(0))
  }

  test("write and apply round trip at start, middle, and end") {
    val storage = freshStorage()
    storage.write(StorageMap.Address.HRAM_START, UByte(0x11))
    storage.write(MID, UByte(0x22))
    storage.write(StorageMap.Address.HRAM_END, UByte(0x33))
    assertEquals(storage(StorageMap.Address.HRAM_START), UByte(0x11))
    assertEquals(storage(MID), UByte(0x22))
    assertEquals(storage(StorageMap.Address.HRAM_END), UByte(0x33))
  }

  test("addresses hold independent values") {
    val storage = freshStorage()
    storage.write(StorageMap.Address.HRAM_START, UByte(0x42))
    storage.write(MID, UByte(0x43))
    assertEquals(storage(StorageMap.Address.HRAM_START), UByte(0x42))
    assertEquals(storage(MID), UByte(0x43))
  }

  test("rejects address it does not own") {
    val storage = freshStorage()
    intercept[IllegalArgumentException] {
      storage(UShort(0xFF7F))
    }
    intercept[IllegalArgumentException] {
      storage(UShort(0xFFFF))
    }
    intercept[IllegalArgumentException] {
      storage.write(UShort(0xFFFF), UByte(0x00))
    }
  }
}

object StorageMapTests {

  val MID: UShort = UShort(0xFFBF)

  def freshStorage(): StorageMap =
    StorageMap(StorageMap.Address.HRAM_START, StorageMap.Address.HRAM_END)
}
