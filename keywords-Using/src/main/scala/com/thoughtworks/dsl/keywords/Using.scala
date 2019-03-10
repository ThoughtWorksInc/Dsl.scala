package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Catch.CatchDsl

import scala.language.implicitConversions
import scala.util.Try

/**
  * @author 杨博 (Yang Bo)
  */
final case class Using[R <: AutoCloseable](open: () => R) extends AnyVal with Keyword[Using[R], R]

/**
  *
  * @example
  */
object Using {

  implicit def implicitUsing[R <: AutoCloseable](r: => R): Using[R] = Using[R](r _)

  def apply[R <: AutoCloseable](r: => R)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): Using[R] = new Using(r _)

  implicit def throwableContinuationUsingDsl[Domain, Value, R <: AutoCloseable](
      implicit catchDsl: CatchDsl[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Using[R], Domain !! Value, R] =
    new Dsl[Using[R], Domain !! Value, R] {
      def cpsApply(keyword: Using[R], handler: R => Domain !! Value): Domain !! Value = _ {
        val r = keyword.open()
        try {
          !Shift(handler(r))
        } finally {
          r.close()
        }
      }

    }

}
