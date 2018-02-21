package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
final case class Yield[Element](element: Element) extends AnyVal with Instruction[Yield[Element], Unit]

object Yield {

  implicit def yieldDsl[Element]: Dsl[Yield[Element], Stream[Element], Unit] =
    new Dsl[Yield[Element], Stream[Element], Unit] {
      def interpret(instruction: Yield[Element], mapper: Unit => Stream[Element]): Stream[Element] = {
        new Stream.Cons(instruction.element, mapper(()))
      }
    }
}
