package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.concurrent.Await.result
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

/** [[Await]] is a [[Dsl.Keyword Keyword]] to extract value from a [[scala.concurrent.Future]].
  *
  * This keyword is available in functions whose return types are
  * [[scala.concurrent.Future Future]], [[domains.task.Task]],
  * or any exception aware continuations as `(_ !! Throwable !! _)`.
  *
  * @example Given a [[scala.concurrent.Future Future]]:
  *          {{{
  *          import scala.concurrent.Future
  *          val myFuture = Future {
  *            42
  *          }
  *          }}}
  *
  *          It can be converted to a [[domains.task.Task]]
  *          with the help of [[Return]] and [[Await]].
  *          {{{
  *          import com.thoughtworks.dsl.domains.task._
  *          import com.thoughtworks.dsl.keywords._
  *          val myTask = Task {
  *            !Await(myFuture)
  *          }
  *          }}}
  *
  *          Then a [[domains.task.Task]] can be converted back to a [[scala.concurrent.Future]]
  *          via [[domains.task.Task.toFuture]].
  *
  *          {{{
  *          val myAssertionTask = Task {
  *            !Shift(myTask) should be(42)
  *          }
  *          Task.toFuture(myAssertionTask)
  *          }}}
  * @author 杨博 (Yang Bo)
  */
final case class Await[Value](future: Future[Value]) extends AnyVal with Keyword[Await[Value], Value]

object Await {

  implicit def implicitAwait[Value](future: Future[Value]): Await[Value] = Await[Value](future)

  implicit def streamAwaitDsl[Value, That](
      implicit executionContext: ExecutionContext): Dsl[Await[Value], Stream[Future[That]], Value] =
    new Dsl[Await[Value], Stream[Future[That]], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Stream[Future[That]]): Stream[Future[That]] = {
        import keyword.future
        val futureOfStream = future.map(handler)
        new Stream.Cons(futureOfStream.flatMap(_.head), result(futureOfStream, Duration.Inf).tail)
      }
    }

  implicit def awaitDsl[Value, That](
      implicit executionContext: ExecutionContext): Dsl[Await[Value], Future[That], Value] =
    new Dsl[Await[Value], Future[That], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Future[That]): Future[That] = {
        keyword.future.flatMap(handler)
      }
    }

  implicit def continuationAwaitDsl[Value](
      implicit executionContext: ExecutionContext): Dsl[Await[Value], Unit !! Throwable, Value] =
    new Dsl[Await[Value], Unit !! Throwable, Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Unit !! Throwable): Unit !! Throwable =
        !!.fromTryContinuation(keyword.future.onComplete)(handler)
    }

}
