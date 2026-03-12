package org.akaii.s4gb.emulator.memorymap

import org.akaii.s4gb.emulator.hashops.*
import org.akaii.s4gb.emulator.memorymap.MemoryMap
import spire.math.{UByte, UShort}

case class TestMap() extends MemoryMap {
  val underlying: Array[UByte] = Array.fill(MemoryMap.MEMORY_SIZE)(UByte.MinValue)

  override def apply(address: UShort): UByte =
    underlying(address.toInt)

  override def write(address: UShort, value: UByte): Unit =
    underlying.update(address.toInt, value)

  override def equals(obj: Any): Boolean = obj match {
    case that: TestMap => this.underlying.sameElements(that.underlying)
    case _ => false
  }

  override def hashCode(): Int = computeHash(underlying.map(_.toInt))
  
  override def toString: String = {
    val populatedEntries = underlying.zipWithIndex
      .filter { (byte, addr) => byte != UByte.MinValue }
      .map { (byte, addr) => f"0x$addr%04X: 0x${byte.toInt}%02X" }
    
    if (populatedEntries.isEmpty) {
      "TestMap()"
    } else {
      s"TestMap(${populatedEntries.mkString(", ")})"
    }
  }
}
