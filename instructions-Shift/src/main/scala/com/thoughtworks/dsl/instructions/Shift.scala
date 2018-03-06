package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Instruction}

import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Shift[Domain, Value](continuation: Domain !! Value)
    extends AnyVal
    with Instruction[Shift[Domain, Value], Value]

private[instructions] trait LowPriorityShift {

  implicit def stackUnsafeShiftDsl[Domain, Value]: Dsl[Shift[Domain, Value], Domain, Value] =
    new Dsl[Shift[Domain, Value], Domain, Value] {
      def interpret(shift: Shift[Domain, Value], mapper: Value => Domain): Domain =
        shift.continuation(mapper)
    }
}

object Shift extends LowPriorityShift {
  trait StackSafeShiftDsl[Domain, Value] extends Dsl[Shift[Domain, Value], Domain, Value]

  implicit def implicitShift[Domain, Value](fa: Domain !! Value): Shift[Domain, Value] =
    Shift[Domain, Value](fa)

  @inline
  implicit def stackSafeShiftDsl[Domain, Value](
      implicit stackSafeShiftDsl: StackSafeShiftDsl[Domain, Value]): Dsl[Shift[Domain, Value], Domain, Value] = {
    stackSafeShiftDsl
  }

}
