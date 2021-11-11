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
  *          val myFuture40 = Future {
  *            40
  *          }
  *          }}}
  *
  *          You can [[Await]] the [[scala.concurrent.Future Future]] in another [[scala.concurrent.Future Future]]
  *
  *          {{{
  *          def myFuture42 = Future {
  *            !Await(myFuture40) + 2
  *          }
  *          }}}
  *
  *          A [[scala.concurrent.Future Future]] can be converted to a [[domains.task.Task]]
  *          with the help of [[Await]].
  *
  *          {{{
  *          import com.thoughtworks.dsl.domains.task.Task
  *          import com.thoughtworks.dsl.keywords.Await
  *          val myTask = Task {
  *            !Await(myFuture42)
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
  * @example `!Await` can be used together with `try` / `catch` / `finally`.
  *          {{{
  *          import scala.concurrent.Future
  *          val buffer = new StringBuffer
  *          def recoverFuture = Future {
  *            buffer.append("Oh")
  *          }
  *          def exceptionalFuture = Future[StringBuffer] {
  *            throw new IllegalStateException("No")
  *          }
  *          def myFuture = Future {
  *            try {
  *              !Await(exceptionalFuture)
  *            } catch {
  *              case e: IllegalStateException =>
  *                !Await(recoverFuture)
  *                buffer.append(' ')
  *                buffer.append(e.getMessage)
  *            } finally {
  *              buffer.append("!")
  *            }
  *          }
  *          myFuture.map(_.toString should be("Oh No!"))
  *          }}}
  * @example Other keywords, including [[Return]] or [[Get]], can be used together with [[Await]]
  *          {{{
  *          import scala.concurrent.Future
  *          import com.thoughtworks.dsl.keywords.{Get, Return}
  *          val buffer = new StringBuffer
  *          def recoverFuture = Future {
  *            buffer.append("Oh")
  *          }
  *          def exceptionalFuture = Future[StringBuffer] {
  *            throw new IllegalStateException("No")
  *          }
  *          def myFuture: Char => Future[StringBuffer] = !Return {
  *            try {
  *              !Await(exceptionalFuture)
  *            } catch {
  *              case e: IllegalStateException =>
  *                !Await(recoverFuture)
  *                buffer.append(!Get[Char])
  *                buffer.append(e.getMessage)
  *            } finally {
  *              buffer.append("!")
  *            }
  *          }
  *          myFuture(' ').map(_.toString should be("Oh No!"))
  *          }}}
  * @author 杨博 (Yang Bo)
  */
final case class Await[Value](future: Future[Value]) extends AnyVal with Keyword[Await[Value], Value]

object Await {

  implicit def implicitAwait[Value](future: Future[Value]): Await[Value] = Await[Value](future)

  implicit def streamAwaitDsl[Value, That](implicit
      executionContext: ExecutionContext
  ): Dsl[Await[Value], Stream[Future[That]], Value] =
    new Dsl[Await[Value], Stream[Future[That]], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Stream[Future[That]]): Stream[Future[That]] = {
        import keyword.future
        val futureOfStream = future.map(handler)
        new Stream.Cons(futureOfStream.flatMap(_.head), result(futureOfStream, Duration.Inf).tail)
      }
    }

  implicit def awaitDsl[Value, That](implicit
      executionContext: ExecutionContext
  ): Dsl[Await[Value], Future[That], Value] =
    new Dsl[Await[Value], Future[That], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Future[That]): Future[That] = {
        keyword.future.flatMap(handler)
      }
    }

  implicit def continuationAwaitDsl[Value](implicit
      executionContext: ExecutionContext
  ): Dsl[Await[Value], Unit !! Throwable, Value] =
    new Dsl[Await[Value], Unit !! Throwable, Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Unit !! Throwable): Unit !! Throwable =
        !!.fromTryContinuation(keyword.future.onComplete)(handler)
    }

}
