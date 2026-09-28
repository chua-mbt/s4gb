package org.akaii.s4gb.collections

trait Preallocated[A] {
  def allocate: A
  def copyInto(target: A, source: A): Unit
}

object Preallocated {
  def apply[A](using ev: Preallocated[A]): Preallocated[A] = ev
}
