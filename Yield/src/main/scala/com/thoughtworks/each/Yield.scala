package com.thoughtworks.each

/**
  * @author 杨博 (Yang Bo)
  */
final case class Yield[Element](element: Element)
    extends /* AnyVal with */ Continuation.InstructionOps[Yield[Element], Unit] {
  protected def self: Yield[Element] = this
}

object Yield {

  implicit def yieldCps[Element]: Continuation[Yield[Element], Stream[Element], Unit] =
    new Continuation[Yield[Element], Stream[Element], Unit] {
      def cpsApply(self: Yield[Element], mapper: Unit => Stream[Element]): Stream[Element] = {
        new Stream.Cons(self.element, mapper(()))
      }
    }
}
