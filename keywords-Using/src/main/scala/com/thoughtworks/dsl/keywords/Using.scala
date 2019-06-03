package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Catch.{CatchDsl, DslCatch}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
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

  def apply[R <: AutoCloseable](r: => R)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): Using[R] = new Using(r _)

  implicit def throwableContinuationUsingDsl[Domain, Value, R <: AutoCloseable](
      implicit catchDsl: DslCatch[Domain, Domain, Value],
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

  @deprecated("Use Dsl[Catch[...], ...] as implicit parameters instead of CatchDsl[...]", "Dsl.scala 1.2.0")
  private[Using] def throwableContinuationUsingDsl[Domain, Value, R <: AutoCloseable](
      implicit catchDsl: CatchDsl[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Using[R], Domain !! Value, R] = {
    throwableContinuationUsingDsl(catchDsl: DslCatch[Domain, Domain, Value],
                                  shiftDsl: Dsl[Shift[Domain, Value], Domain, Value])
  }

  implicit def scalaFutureUsingDsl[R <: AutoCloseable, A](implicit executionContext: ExecutionContext)
    : Dsl[Using[R], Future[A], R] = { (keyword: Using[R], handler: R => Future[A]) =>
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
        val fa: Future[A] = try {
          handler(r)
        } catch {
          case NonFatal(e) =>
            return onFailure(e)
        }
        fa.recoverWith {
            case NonFatal(e) =>
              onFailure(e)
          }
          .flatMap(onSuccess)
      }
      returnableBlock()
    }
  }
}
