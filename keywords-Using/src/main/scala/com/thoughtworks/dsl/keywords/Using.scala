package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.bangnotation.{ `*`, reify, reset, unary_!}
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.Dsl.IsKeyword
// import com.thoughtworks.dsl.keywords.Catch.{CatchDsl, DslCatch}
import com.thoughtworks.dsl.keywords.TryFinally
import com.thoughtworks.dsl.Dsl.cpsApply

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** This [[Using]] keyword automatically manage resources in [[scala.concurrent.Future]], [[domains.task.Task]],
  * and other asynchronous domains derived from `Future` or `Task`.
  *
  * @author 杨博 (Yang Bo)
  * @see [[dsl]] for usage of this [[Using]] keyword in continuations
  */
final case class Using[R <: AutoCloseable](open: () => R) extends AnyVal

object Using {
  given [R <: AutoCloseable]: IsKeyword[Using[R], R] with {}

  implicit def implicitUsing[R <: AutoCloseable](r: => R): Using[R] = Using[R](() => r)

  trait ScopeExitHandler extends AutoCloseable

  /** Returns a [[Using]] keyword to execute a [[ScopeExitHandler]] when exiting the nearest enclosing scope
    * that is annotated as [[Dsl.reset @reset]],
    * (or the nearest enclosing function if [[compilerplugins.ResetEverywhere]] is enabled).
    *
    * @note This method is similar to [[apply]],
    *       except the parameter type is changed from a generic `R` to the SAM type [[ScopeExitHandler]],
    *       which allows for function literal expressions
    *       in Scala 2.12+ or Scala 2.11 with `-Xexperimental` compiler option.
    *
    * @example The following function will perform `n *= 2` after `n += 20`:
    *
    *          {{{
    *          import scala.concurrent.Future
    *          import com.thoughtworks.dsl.keywords.Using.scopeExit
    *          var n = 1
    *          def multiplicationAfterAddition = Future {
    *            !scopeExit { () =>
    *              n *= 2
    *            }
    *            n += 20
    *          }
    *          }}}
    *
    *          Therefore, the final value of `n` should be `(1 + 20) * 2 = 42`.
    *
    *          {{{
    *          multiplicationAfterAddition.map { _ =>
    *            n should be(42)
    *          }
    *          }}}
    */
  def scopeExit(r: => ScopeExitHandler) = new Using(() => r)

  def apply[R <: AutoCloseable](r: => R)(implicit
      dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
  ): Using[R] = new Using(() => r)

  implicit def continuationUsingDsl[Domain, Value, R <: AutoCloseable](implicit
      tryFinallyDsl: Dsl[TryFinally[Suspend[Shift[Domain, Value]], Pure[scala.Unit]], Domain !! Value, Value],
      // shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Using[R], Domain !! Value, R] = { (keyword: Using[R], handler: R => Domain !! Value) =>
    *[[X] =>> Domain !! X] {
      val r = keyword.open()
      try {
        !Shift[Domain, Value](handler(r))
      } finally {
        r.close()
      }
    }
  }

}
