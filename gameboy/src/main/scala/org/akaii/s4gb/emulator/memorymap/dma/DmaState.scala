package org.akaii.s4gb.emulator.memorymap.dma

import spire.math.UByte

/**
 * Shared DMA status between the register, the gate, and the transfer component.
 */
final class DmaState(
  var isActive: Boolean = false,
  var sourceHighByte: UByte = UByte(0),
  var dot: Int = 0,
)
