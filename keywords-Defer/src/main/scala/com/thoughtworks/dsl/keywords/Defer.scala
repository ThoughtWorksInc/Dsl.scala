package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Catch.DslCatch
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** This [[Defer]] keyword automatically manage resources in [[scala.concurrent.Future]], [[domains.task.Task]],
  * and other asynchrounous domains derived from `Future` or `Task`.
  *
  * @see [[dsl]] for usage of this [[Defer]] keyword in continuations
  */
final case class Defer(op: () => Unit) extends AnyVal with Keyword[Defer, Unit]

trait LowPriorityDefer {

  implicit def deferDsl[Domain]: Dsl[Defer, Domain, Unit] =
    new Dsl[Defer, Domain, Unit] {
      def cpsApply(keyword: Defer, handler: Unit => Domain): Domain = {
        try {
          handler(())
        } finally {
          keyword.op()
        }
      }
    }

}

object Defer extends LowPriorityDefer {

  implicit def implicitDefer(r: => Unit): Defer = Defer(r _)

  def apply(r: => Unit)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): Defer = new Defer(r _)


  implicit def throwableContinuationDeferDsl[Domain, Value, R <: AutoCloseable](
      implicit catchDsl: DslCatch[Domain, Domain, Value],
      shiftDsl: Dsl[Shift[Domain, Value], Domain, Value]
  ): Dsl[Defer, Domain !! Value, Unit] =
    new Dsl[Defer, Domain !! Value, Unit] {
      def cpsApply(keyword: Defer, handler: Unit => Domain !! Value): Domain !! Value = _ {
        try {
          !Shift(handler(()))
        } finally {
          keyword.op()
        }
      }
    }

  implicit def scalaFutureDeferDsl[A](
      implicit executionContext: ExecutionContext): Dsl[Defer, Future[A], Unit] =
    new Dsl[Defer, Future[A], Unit] {
      def cpsApply(keyword: Defer, handler: Unit => Future[A]): Future[A] = {
        val f = try {
          handler(())
        } catch {
          case NonFatal(e) =>
            Future.failed(e)
        }
        val p = Promise[A]()
        f.onComplete { t =>
          try {
            keyword.op()
            p.complete(t)
          } catch {
            case NonFatal(e) =>
              p.failure(e)
          }
        }
        p.future
      }
    }

}
