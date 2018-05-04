package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Catch.CatchDsl

import scala.language.implicitConversions
import scala.util.Try

/**
  * @author 杨博 (Yang Bo)
  */
final case class AutoClose[R <: AutoCloseable](open: () => R) extends AnyVal with Keyword[AutoClose[R], R]

/**
  *
  * @example
  */
object AutoClose {

  implicit def implicitAutoClose[R <: AutoCloseable](r: => R): AutoClose[R] = AutoClose[R](r _)

  def apply[R <: AutoCloseable](r: => R)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): AutoClose[R] = new AutoClose(r _)

  implicit def throwableContinuationAutoCloseDsl[Domain, Value, R <: AutoCloseable](
      implicit catchDsl: CatchDsl[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[AutoClose[R], Domain !! Value, R] =
    new Dsl[AutoClose[R], Domain !! Value, R] {
      def interpret(keyword: AutoClose[R], handler: R => Domain !! Value): Domain !! Value = _ {
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
