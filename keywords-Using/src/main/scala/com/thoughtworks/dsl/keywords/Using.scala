package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Catch.{CatchDsl, DslCatch}
import com.thoughtworks.dsl.Dsl.TryFinally

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** This [[Using]] keyword automatically manage resources in [[scala.concurrent.Future]], [[domains.task.Task]],
  * and other asynchrounous domains derived from `Future` or `Task`.
  *
  * @author 杨博 (Yang Bo)
  * @see [[dsl]] for usage of this [[Using]] keyword in continuations
  */
final case class Using[R <: AutoCloseable](open: () => R) extends AnyVal with Keyword[Using[R], R]

object Using {

  implicit def implicitUsing[R <: AutoCloseable](r: => R): Using[R] = Using[R](r _)

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
  def scopeExit(r: => ScopeExitHandler) = new Using(r _)

  def apply[R <: AutoCloseable](r: => R)(implicit
      dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
  ): Using[R] = new Using(r _)

  @deprecated("[[keywords.Catch]] will be removed in favor of [[Dsl.TryCatch]].", "Dsl.scala 1.4.0")
  private[Using] def throwableContinuationUsingDsl[Domain, Value, R <: AutoCloseable](implicit
      catchDsl: DslCatch[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Using[R], Domain !! Value, R] = {
    (keyword: Using[R], handler: R => Domain !! Value) => (outerHandler: Value => Domain) =>
      val r = keyword.open()
      Catch
        .tryCatch { value: Value =>
          r.close()
          outerHandler(value)
        }
        .apply(
          Shift(handler(r)).cpsApply(_),
          { case NonFatal(e) =>
            r.close()
            _ {
              throw e
            }
          }
        )
  }

  implicit def continuationUsingDsl[Domain, Value, R <: AutoCloseable](implicit
      tryFinally: TryFinally[Value, Domain, Domain, Domain],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Using[R], Domain !! Value, R] = { (keyword: Using[R], handler: R => Domain !! Value) =>
    _ {
      val r = keyword.open()
      try {
        !Shift(handler(r))
      } finally {
        r.close()
      }
    }
  }

  @deprecated("[[keywords.Catch]] will be removed in favor of [[Dsl.TryCatch]].", "Dsl.scala 1.2.0")
  private[Using] def throwableContinuationUsingDsl[Domain, Value, R <: AutoCloseable](implicit
      catchDsl: CatchDsl[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Using[R], Domain !! Value, R] = {
    throwableContinuationUsingDsl(
      catchDsl: DslCatch[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
    )
  }

  implicit def scalaFutureUsingDsl[R <: AutoCloseable, A](implicit
      executionContext: ExecutionContext
  ): Dsl[Using[R], Future[A], R] = { (keyword: Using[R], handler: R => Future[A]) =>
    Future(keyword.open()).flatMap { r: R =>
      def onFailure(e: Throwable): Future[Nothing] = {
        try {
          r.close()
          Future.failed(e)
        } catch {
          case NonFatal(e2) =>
            Future.failed(e2)
        }
      }

      def onSuccess(a: A): Future[A] = {
        try {
          r.close()
          Future.successful(a)
        } catch {
          case NonFatal(e2) =>
            Future.failed(e2)
        }
      }

      def returnableBlock(): Future[A] = {
        val fa: Future[A] =
          try {
            handler(r)
          } catch {
            case NonFatal(e) =>
              return onFailure(e)
          }
        fa.recoverWith { case NonFatal(e) =>
          onFailure(e)
        }.flatMap(onSuccess)
      }
      returnableBlock()
    }
  }
}
