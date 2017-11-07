package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
final case class Await[Domain, A](continuation: (A => Domain) => Domain)
    extends AnyVal
    with Instruction[Await[Domain, A], A]

object Await {

  implicit def awaitDsl[Domain, A]: Dsl[Await[Domain, A], Domain, A] =
    new Dsl[Await[Domain, A], Domain, A] {
      def interpret(self: Await[Domain, A], mapper: A => Domain): Domain = self.continuation(mapper)
    }

}
