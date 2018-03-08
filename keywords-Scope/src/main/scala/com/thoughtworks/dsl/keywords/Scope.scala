package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Scope[Domain, Value](continuation: Domain !! Value)
    extends AnyVal
    with Keyword[Scope[Domain, Value], Value]
object Scope {

  implicit def implicitScope[Domain, Value](continuation: Domain !! Value): Scope[Domain, Value] =
    Scope[Domain, Value](continuation)

  implicit def scopeContinuationDsl[Domain, DomainValue, ScopeValue](
      implicit restScopeDsl: Dsl[Scope[Domain, DomainValue], Domain, DomainValue])
    : Dsl[Scope[Domain !! DomainValue, ScopeValue], Domain !! DomainValue, ScopeValue] with Object =
    new Dsl[Scope[Domain !! DomainValue, ScopeValue], Domain !! DomainValue, ScopeValue] {

      def interpret(keyword: Scope[Domain !! DomainValue, ScopeValue],
                    handler: ScopeValue => Domain !! DomainValue): Domain !! DomainValue = {
        restScopeDsl.interpret(Scope[Domain, DomainValue](keyword.continuation(handler)), _)
      }
    }
}
