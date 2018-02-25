package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Shift[Domain, A](continuation: Shift.Continuation[Domain, A])
    extends AnyVal
    with Instruction[Shift[Domain, A], A]

object Shift {

  type Continuation[Domain, +A] = (A => Domain) => Domain

  implicit def implicitShift[Domain, A](fa: Continuation[Domain, A]): Shift[Domain, A] = Shift[Domain, A](fa)

  implicit def shiftDsl[Domain, A]: Dsl[Shift[Domain, A], Domain, A] =
    new Dsl[Shift[Domain, A], Domain, A] {
      def interpret(shift: Shift[Domain, A], mapper: A => Domain): Domain = shift.continuation(mapper)
    }

}
