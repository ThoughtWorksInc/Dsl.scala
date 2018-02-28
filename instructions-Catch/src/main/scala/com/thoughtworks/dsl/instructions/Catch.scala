package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.util.control.Exception.Catcher

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](onFailure: Throwable => Domain)
    extends AnyVal
    with Instruction[Catch[Domain], Domain => Domain]

object Catch {

  implicit def catchContinuationDsl[Domain, Value](implicit restCatchDsl: Dsl[Catch[Domain], Domain, Domain => Domain])
    : Dsl[Catch[(Value => Domain) => Domain],
          (Value => Domain) => Domain,
          ((Value => Domain) => Domain) => ((Value => Domain) => Domain)] =
    new Dsl[Catch[(Value => Domain) => Domain],
            (Value => Domain) => Domain,
            ((Value => Domain) => Domain) => ((Value => Domain) => Domain)] {
      def interpret(
          catcher: Catch[(Value => Domain) => Domain],
          block: (((Value => Domain) => Domain) => (Value => Domain) => Domain) => (Value => Domain) => Domain)
        : (Value => Domain) => Domain = _ {
        try {
          !Shift(block(identity)) // TODO: catch exception in block(identity) ?
        } catch {
          case e: Throwable =>
            !Shift(catcher.onFailure(e))
        }
      }
    }
}
