package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.implicitConversions
import scala.util.Try

/**
  * @author 杨博 (Yang Bo)
  */
final case class AutoClose[R <: AutoCloseable](open: () => R) extends AnyVal with Keyword[AutoClose[R], R]

trait LowPriorityAutoClose {
//  implicit def continuationAutoCloseDsl[Domain, ScopeValue, R <: AutoCloseable](
//      implicit shiftDsl: Dsl[Shift[Domain, ScopeValue], Domain, ScopeValue])
//    : Dsl[AutoClose[R], Domain !! ScopeValue, R] =
//    new Dsl[AutoClose[R], Domain !! ScopeValue, R] {
//      def interpret(keyword: AutoClose[R], handler: R => Domain !! ScopeValue): Domain !! ScopeValue = _ {
//        val r = keyword.open()
//        val scopeResult = !Shift(handler(r))
//        r.close()
//        scopeResult
//      }
//    }
}

/**
  *
  * @example
  */
object AutoClose extends LowPriorityAutoClose {

  implicit def implicitAutoClose[R <: AutoCloseable](r: => R): AutoClose[R] = AutoClose[R](r _)

  def apply[R <: AutoCloseable](r: => R)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): AutoClose[R] = new AutoClose(r _)

  // TODO: Dsl for try/catch
  implicit def throwableContinuationAutoCloseDsl[Domain, ScopeValue, R <: AutoCloseable](
      implicit shiftDsl: Dsl[Shift[Domain, ScopeValue], Domain, ScopeValue],
      catchDsl: Dsl[Catch[Domain], Domain, Unit],
      scopeDsl: Dsl[Scope[Domain, Try[ScopeValue]], Domain, Try[ScopeValue]]
  ): Dsl[AutoClose[R], Domain !! ScopeValue, R] =
    new Dsl[AutoClose[R], Domain !! ScopeValue, R] {
      def interpret(keyword: AutoClose[R], handler: R => Domain !! ScopeValue): Domain !! ScopeValue = _ {
        val r = keyword.open()
        val scopeResult = try {
          !Shift(handler(r))
        } finally {
          r.close()
        }
        scopeResult
      }

    }

}
