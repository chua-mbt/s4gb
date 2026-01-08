package org.akaii.s4gb.emulator

case class TestMap() extends MemoryMap {
  private val memory: Array[Int] = Array.fill(MemoryMap.MEMORY_SIZE)(0)

  override def apply(address: Int): Int = {
    memory(address & 0xFFFF)
  }

  override def write(address: Int, value: Int): Unit = {
    memory(address & 0xFFFF) = value
  }
}
