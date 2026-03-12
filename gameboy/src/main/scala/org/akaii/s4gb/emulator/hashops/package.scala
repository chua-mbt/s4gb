package org.akaii.s4gb.emulator

package object hashops {
  /**
   * Computes a hash code using primes:
   * s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]
   *
   * Horner's Rule (above rewritten) allows us to compute this in a single pass e.g.:
   * `a * 31^2 + b * 31 + c  ==  ((a*31 + b)*31 + c)`
   *
   * @see [[https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/lang/String.java#L2702-L2714]]
   * @see [[https://en.wikipedia.org/wiki/Horner%27s_method]]
   */
  def computeHash(input: Array[Int]): Int = {
    val prime = 31
    input.foldLeft(0)((hash, n) => hash * prime + n)
  }
}
