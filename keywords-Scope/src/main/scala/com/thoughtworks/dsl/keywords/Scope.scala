package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Scope[Domain, Value](continuation: Domain !! Value)
    extends AnyVal
    with Keyword[Scope[Domain, Value], Value]

private[keywords] trait LowPriorityScope0 { this: Scope.type =>

  implicit def scopeContinuationDsl[Domain, DomainValue, ScopeValue](
      implicit restScopeDsl: Dsl[Scope[Domain, DomainValue], Domain, DomainValue])
    : Dsl[Scope[Domain !! DomainValue, ScopeValue], Domain !! DomainValue, ScopeValue] =
    new Dsl[Scope[Domain !! DomainValue, ScopeValue], Domain !! DomainValue, ScopeValue] {

      def interpret(keyword: Scope[Domain !! DomainValue, ScopeValue],
                    handler: ScopeValue => Domain !! DomainValue): Domain !! DomainValue = {
        restScopeDsl.interpret(Scope[Domain, DomainValue](keyword.continuation(handler)), _)
      }
    }

}

object Scope extends LowPriorityScope0 {

  implicit def implicitScope[Domain, Value](continuation: Domain !! Value): Scope[Domain, Value] =
    Scope[Domain, Value](continuation)

  implicit def throwableScopeDsl[Domain, ScopeValue]
    : Dsl[Scope[Domain !! Throwable, ScopeValue], Domain !! Throwable, ScopeValue] =
    new Dsl[Scope[Domain !! Throwable, ScopeValue], Domain !! Throwable, ScopeValue] {
      def interpret(scope: Scope[Domain !! Throwable, ScopeValue],
                    rest: ScopeValue => Domain !! Throwable): Domain !! Throwable = { outerFailureHandler =>
        @inline
        def jvmCatch(block: => Domain !! Throwable)(failureHandler: Throwable => Domain): Domain = {
          (try {
            block
          } catch {
            case NonFatal(e) =>
              return failureHandler(e)
          }).apply(failureHandler)
        }

        jvmCatch(scope.continuation { (scopeValue: ScopeValue) => (_: Throwable => Domain) =>
          jvmCatch(rest(scopeValue))(outerFailureHandler)
        })(outerFailureHandler)

      }
    }

}
