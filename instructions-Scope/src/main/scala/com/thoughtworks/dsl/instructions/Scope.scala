package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
final case class Scope[Domain, Value](continuation: (Value => Domain) => Domain)
    extends AnyVal
    with Instruction[Scope[Domain, Value], Value]
object Scope {

  implicit def scopeContinuationDsl[Domain, DomainValue, ScopeValue](
      implicit restScopeDsl: Dsl[Scope[Domain, DomainValue], Domain, DomainValue]): Dsl[
    Scope[(DomainValue => Domain) => Domain, ScopeValue],
    (DomainValue => Domain) => Domain,
    ScopeValue] with Object =
    new Dsl[Scope[(DomainValue => Domain) => Domain, ScopeValue], (DomainValue => Domain) => Domain, ScopeValue] {

      def interpret(instruction: Scope[(DomainValue => Domain) => Domain, ScopeValue],
                    handler: ScopeValue => (DomainValue => Domain) => Domain): (DomainValue => Domain) => Domain = {
        restScopeDsl.interpret(Scope[Domain, DomainValue](instruction.continuation(handler)), _)
      }
    }
}
