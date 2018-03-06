package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Instruction}
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](failureHandler: Throwable => Domain) extends AnyVal with Instruction[Catch[Domain], Unit]

object Catch {

  implicit def implicitCatch[Domain](onFailure: Throwable => Domain): Catch[Domain] = Catch[Domain](onFailure)

  implicit def catchContinuationDsl[Domain, Value](
      implicit restCatchDsl: Dsl[Catch[Domain], Domain, Unit]): Dsl[Catch[Domain !! Value], Domain !! Value, Unit] =
    new Dsl[Catch[Domain !! Value], Domain !! Value, Unit] {

      def interpret(instruction: Catch[Domain !! Value], block: Unit => Domain !! Value): Domain !! Value = {
        (continue: Value => Domain) =>
          restCatchDsl.interpret(
            Catch[Domain] { e =>
              instruction.failureHandler(e)(continue)
            }, { _: Unit =>
              block(())(continue)
            }
          )
      }
    }

}
