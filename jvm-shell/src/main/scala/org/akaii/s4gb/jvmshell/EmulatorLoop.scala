package org.akaii.s4gb.jvmshell

import org.akaii.s4gb.emulator.components.Joypad.{Button, ButtonState}
import org.akaii.s4gb.emulator.components.Rom
import org.akaii.s4gb.emulator.components.ppu.Ppu
import org.akaii.s4gb.emulator.{Config, Emulator}
import spire.math.UByte

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.ArraySeq

/**
 * Emulation thread. One iteration is one frame of hardware time, paced against absolute
 * deadlines so the loop does not drift, with the frame published to the screen on the
 * LY wrap and again at the end of the budget if the LCD never wraps.
 */
final class EmulatorLoop(
  screen: ScreenPanel,
  onFrameRate: Double => Unit
) {

  import EmulatorLoop.*

  @volatile private var machine: Option[Emulator] = None
  @volatile private var running = false

  /** Key events land here from the EDT and are applied inside `tickFrame`. */
  private val pending = new ConcurrentLinkedQueue[(Button, ButtonState)]()

  def setButton(button: Button, state: ButtonState): Unit =
    pending.add((button, state))

  def load(romData: ArraySeq[Byte]): Unit = {
    screen.clear()
    pending.clear()
    val masked = romData.toArray.map(b => (b & 0xFF).toByte)
    val fresh = Emulator(Rom(masked.map(UByte(_))), screen, Config())
    fresh.initialize()
    machine = Some(fresh)
  }

  def start(): Unit = synchronized {
    if (running) return
    running = true
    val thread = new Thread(() => run(), "s4gb-emulator")
    thread.setDaemon(true)
    thread.start()
  }

  def stop(): Unit = running = false

  private def run(): Unit = {
    val start = System.nanoTime()
    run(start, start, 0)
  }

  /**
   * One iteration per frame. `deadline` is when the next iteration should finish, so it is
   * advanced before the wait rather than after, otherwise the sleep is always skipped.
   * Falling behind drops the debt instead of catching up.
   */
  @annotation.tailrec
  private def run(deadline: Long, windowStart: Long, windowFrames: Int): Unit =
    if (!running) ()
    else {
      machine match {
        case Some(active) => tickFrame(active)
        case None         => pending.clear()
      }

      val now = System.nanoTime()
      val advanced = deadline + NanosPerFrame
      val nextDeadline =
        if (advanced > now) {
          pause(advanced)
          advanced
        } else now

      val frames = windowFrames + 1
      val elapsed = now - windowStart

      if (elapsed >= NanosPerSecond) {
        val rate = frames * NanosPerSecond.toDouble / elapsed
        println(f"[frame-rate] $rate%.2f fps")
        onFrameRate(rate)
        run(nextDeadline, now, 0)
      } else run(nextDeadline, windowStart, frames)
    }

  private def tickFrame(machine: Emulator): Unit =
    tickFrame(machine, 0, machine.ppu(Ppu.Address.LY).toInt, presented = false)

  @annotation.tailrec
  private def tickFrame(machine: Emulator, ticks: Int, ly: Int, presented: Boolean): Unit =
    if (ticks >= CyclesPerFrame) {
      if (!presented) screen.present()
    } else {
      if (ticks % InputBatchTicks == 0) drainInput(machine)
      machine.tick()
      val next = machine.ppu(Ppu.Address.LY).toInt
      val shown =
        if (!presented && ly > 0 && next == 0) {
          screen.present()
          true
        } else presented
      tickFrame(machine, ticks + 1, next, shown)
    }

  private def drainInput(machine: Emulator): Unit = drainInput(machine, Option(pending.poll()))

  @annotation.tailrec
  private def drainInput(machine: Emulator, next: Option[(Button, ButtonState)]): Unit =
    next match {
      case Some((button, state)) =>
        machine.joypad.setButtonState(button, state)
        drainInput(machine, Option(pending.poll()))
      case None => ()
    }

  @annotation.tailrec
  private def pause(until: Long): Unit = {
    val remaining = until - System.nanoTime()
    if (remaining <= SpinThreshold) spin(until)
    else {
      Thread.sleep(remaining / NanosPerMilli, (remaining % NanosPerMilli).toInt)
      pause(until)
    }
  }

  @annotation.tailrec
  private def spin(until: Long): Unit =
    if (System.nanoTime() < until) {
      Thread.onSpinWait()
      spin(until)
    }
}

object EmulatorLoop {

  /** 70224 t-cycles per frame at 4.194304 MHz: 59.7275 Hz, 16.743 ms. */
  private val NanosPerFrame: Long = 1_000_000_000L * 70224 / 4194304
  private val NanosPerSecond = 1_000_000_000L
  private val NanosPerMilli = 1_000_000L

  /** `tick` advances one m-cycle, four t-cycles. */
  private val CyclesPerFrame = 70224 / 4

  /** Input is drained this often, in m-cycles: 64 is 0.25 ms of key latency. */
  private val InputBatchTicks = 64

  private val SpinThreshold = 2_000_000L
}
