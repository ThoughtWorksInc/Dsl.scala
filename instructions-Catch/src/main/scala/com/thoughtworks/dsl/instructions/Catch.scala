package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](onFailure: Throwable => Domain) extends AnyVal with Instruction[Catch[Domain], Unit]

object Catch {

  implicit def catchContinuationDsl[Domain, Value](implicit restCatchDsl: Dsl[Catch[Domain], Domain, Unit])
    : Dsl[Catch[(Value => Domain) => Domain], (Value => Domain) => Domain, Unit] =
    new Dsl[Catch[(Value => Domain) => Domain], (Value => Domain) => Domain, Unit] {

      def interpret(
          instruction: Catch[(Value => Domain) => Domain],
          block: Unit => (Value => Domain) => Domain): (Value => Domain) => Domain = { (continue: Value => Domain) =>
        restCatchDsl.interpret(
          Catch[Domain] { e =>
            instruction.onFailure(e)(continue)
          }, { _: Unit =>
            block(())(continue)
          }
        )
      }
    }

}
