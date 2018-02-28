package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Shift[Domain, Value](continuation: (Value => Domain) => Domain)
    extends AnyVal
    with Instruction[Shift[Domain, Value], Value]

object Shift {

  implicit def implicitShift[Domain, Value](fa: (Value => Domain) => Domain): Shift[Domain, Value] =
    Shift[Domain, Value](fa)

  implicit def shiftDsl[Domain, Value]: Dsl[Shift[Domain, Value], Domain, Value] =
    new Dsl[Shift[Domain, Value], Domain, Value] {
      def interpret(shift: Shift[Domain, Value], mapper: Value => Domain): Domain = shift.continuation(mapper)
    }

}
