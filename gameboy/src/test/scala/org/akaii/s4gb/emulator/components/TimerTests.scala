package org.akaii.s4gb.emulator.components

import munit.FunSuite
import org.akaii.s4gb.emulator.byteops.*
import spire.math.{UByte, UShort}

class TimerTests extends FunSuite {

  import TimerTests.*

  test("write and apply round trip for DIV") {
    withTimer { timer =>
      timer.setSystemCounter(UShort(0x1234))
      val divBefore = timer(Timer.Address.DIV)
      assert(divBefore != 0.toUByte)

      timer.write(Timer.Address.DIV, UByte(0xAB))
      assertEquals(timer(Timer.Address.DIV), UByte(0))
    }
  }

  test("write and apply round trip for TIMA") {
    withTimer { timer =>
      timer.write(Timer.Address.TIMA, UByte(0x12))
      assertEquals(timer(Timer.Address.TIMA), UByte(0x12))
    }
  }

  test("write and apply round trip for TMA") {
    withTimer { timer =>
      timer.write(Timer.Address.TMA, UByte(0x34))
      assertEquals(timer(Timer.Address.TMA), UByte(0x34))
    }
  }

  test("write and apply round trip for TAC") {
    withTimer { timer =>
      timer.write(Timer.Address.TAC, UByte(0x07))
      assertEquals(timer(Timer.Address.TAC), UByte(0x07))
    }
  }

  TimerTests.frequencies.foreach { case (freqBits, freqName) =>
    test(s"TIMA increment at $freqName") {
      withTimer { timer =>
        timer.write(Timer.Address.TAC, freqBits.toUByte | Timer.Masks.TAC_ENABLE)
        timer.write(Timer.Address.TIMA, UByte(0x00))

        val watchedBit = Timer.watchedBitForTAC(UByte(freqBits))
        timer.setSystemCounter(UShort((1 << (watchedBit + 1)) - 1))

        val prevTima = timer(Timer.Address.TIMA)
        timer.tick()
        val newTima = timer(Timer.Address.TIMA)
        assertEquals(prevTima, UByte(0))
        assertEquals(newTima, UByte(1))
      }
    }

    test(s"TIMA overflow at $freqName triggers interrupt") {
      withTimer { timer =>
        val interrupts = timer.interrupts
        timer.write(Timer.Address.TAC, UByte(freqBits) | Timer.Masks.TAC_ENABLE)
        timer.write(Timer.Address.TMA, UByte(0x42))
        timer.write(Timer.Address.TIMA, UByte(0xFF))

        val watchedBit = Timer.watchedBitForTAC(UByte(freqBits))
        timer.setSystemCounter(UShort((1 << (watchedBit + 1)) - 1))

        val prevTima = timer(Timer.Address.TIMA)
        timer.tick()
        val newTima = timer(Timer.Address.TIMA)
        assertEquals(prevTima, UByte(0xFF))
        assertEquals(newTima, UByte(0x42))
      }
    }
  }
}

object TimerTests {
  private def withTimer(test: Timer => Unit): Unit = {
    val interrupts = Interrupts()
    val timer = Timer(interrupts)
    test(timer)
  }

  val frequencies: Seq[(Int, String)] = Seq(
    0x00 -> "4096 Hz",
    0x01 -> "262144 Hz",
    0x02 -> "65536 Hz",
    0x03 -> "16384 Hz"
  )

  private val TICKS_TO_NON_ZERO_DIV = 256
}