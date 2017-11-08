package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
final case class Shift[Domain, A](continuation: (A => Domain) => Domain)
    extends AnyVal
    with Instruction[Shift[Domain, A], A]

object Shift {

  implicit def awaitDsl[Domain, A]: Dsl[Shift[Domain, A], Domain, A] =
    new Dsl[Shift[Domain, A], Domain, A] {
      def interpret(self: Shift[Domain, A], mapper: A => Domain): Domain = self.continuation(mapper)
    }

}
