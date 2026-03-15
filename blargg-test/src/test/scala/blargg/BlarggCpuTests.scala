package blargg

import munit.FunSuite

class BlarggCpuTests extends FunSuite {

  import BlarggCpuTests.*

  romFiles.foreach { filename =>
    test(s"load $filename") {
      val is = getClass.getResourceAsStream(s"$resourcePath/$filename")
      assert(is != null, s"Resource $filename not found")
      val bytes = Iterator.continually(is.read()).takeWhile(_ != -1).map(_.toByte).toArray
      is.close()
      assertEquals(bytes.length, 32768, s"ROM $filename should be 32KB")
    }
  }
}

object BlarggCpuTests {
  private val resourcePath = "/cpu_instrs/individual"

  private val romFiles: List[String] = List(
    "01-special.gb",
    "02-interrupts.gb",
    "03-op sp,hl.gb",
    "04-op r,imm.gb",
    "05-op rp.gb",
    "06-ld r,r.gb",
    "07-jr,jp,call,ret,rst.gb",
    "08-misc instrs.gb",
    "09-op r,r.gb",
    "10-bit ops.gb",
    "11-op a,(hl).gb"
  )
}
