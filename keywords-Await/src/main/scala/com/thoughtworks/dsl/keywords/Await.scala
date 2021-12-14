package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.domains.Continuation.!!
import scala.concurrent.Await.result
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

/** [[Await]] is a [[Dsl.Keyword Keyword]] to extract value from a [[scala.concurrent.Future]].
  *
  * This keyword is available in functions whose return types are [[scala.concurrent.Future Future]],
  * [[domains.task.Task]], or any exception aware continuations as `(_ !! Throwable !! _)`.
  *
  * @example
  *   Given a [[scala.concurrent.Future Future]]:
  * {{{
  * import com.thoughtworks.dsl.reset, reset._
  * import scala.concurrent.Future
  * val myFuture40 = Future {
  *   40
  * }
  * }}}
  *
  * You can [[Await]] the [[scala.concurrent.Future Future]] in another [[scala.concurrent.Future Future]]
  *
  * {{{
  * def myFuture42 = *[Future] {
  *   !Await(myFuture40) + 2
  * }
  * }}}
  *
  * A [[scala.concurrent.Future Future]] can be converted to a [[domains.task.Task]] with the help of [[Await]].
  *
  * {{{
  * import com.thoughtworks.dsl.domains.Task
  * import com.thoughtworks.dsl.keywords.Await
  * val myTask = *[Task] {
  *   !Await(myFuture42)
  * }
  * }}}
  *
  * Then a [[domains.task.Task]] can be converted back to a [[scala.concurrent.Future]] via
  * [[domains.task.Task.toFuture]].
  *
  * {{{
  * val myAssertionTask = *[Task] {
  *   !Shift(myTask) should be(42)
  * }
  * Task.toFuture(myAssertionTask)
  * }}}
  * @example
  *   `!Await` can be used together with `try` / `catch` / `finally`.
  * {{{
  * import scala.concurrent.Future
  * import com.thoughtworks.dsl.reset, reset._
  * val buffer = new StringBuffer
  * def recoverFuture = Future {
  *   buffer.append("Oh")
  * }
  * def exceptionalFuture = Future[StringBuffer] {
  *   throw new IllegalStateException("No")
  * }
  * def myFuture = *[Future] {
  *   try {
  *     !Await(exceptionalFuture)
  *   } catch {
  *     case e: IllegalStateException =>
  *       !Await(recoverFuture)
  *       buffer.append(' ')
  *       buffer.append(e.getMessage)
  *   } finally {
  *     buffer.append("!")
  *   }
  * }
  * myFuture.map(_.toString should be("Oh No!"))
  * }}}
  * @example
  *   Other keywords, including [[Return]] or [[Get]], can be used together with [[Await]]
  * {{{
  * import scala.concurrent.Future
  * import com.thoughtworks.dsl.keywords.{Get, Return}
  * import com.thoughtworks.dsl.reset, reset._
  * val buffer = new StringBuffer
  * def recoverFuture = Future {
  *   buffer.append("Oh")
  * }
  * def exceptionalFuture = Future[StringBuffer] {
  *   throw new IllegalStateException("No")
  * }
  * def myFuture = reset[Char => Future[StringBuffer]](!Return {
  *   try {
  *     !Await(exceptionalFuture)
  *   } catch {
  *     case e: IllegalStateException =>
  *       !Await(recoverFuture)
  *       buffer.append(!Get[Char]())
  *       buffer.append(e.getMessage)
  *   } finally {
  *     buffer.append("!")
  *   }
  * })
  * myFuture(' ').map(_.toString should be("Oh No!"))
  * }}}
  * @author
  *   杨博 (Yang Bo)
  */
opaque type Await[Result] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[concurrent.Future[Result]]
object Await {
  @inline def apply[Result]: concurrent.Future[Result] =:= Await[Result] = Dsl.Keyword.Opaque.Of.apply
  given [Result]: AsKeyword.IsKeyword[Await[Result], Result] with {}

  implicit def streamAwaitDsl[Value, That](implicit
      executionContext: ExecutionContext
  ): Dsl[Await[Value], Stream[Future[That]], Value] =
    new Dsl[Await[Value], Stream[Future[That]], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Stream[Future[That]]): Stream[Future[That]] = {
        val futureOfStream = keyword.map(handler)
        new Stream.Cons(futureOfStream.flatMap(_.head), result(futureOfStream, Duration.Inf).tail)
      }
    }

  implicit def awaitDsl[Value, That](implicit
      executionContext: ExecutionContext
  ): Dsl[Await[Value], Future[That], Value] =
    new Dsl[Await[Value], Future[That], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Future[That]): Future[That] = {
        keyword.flatMap(handler)
      }
    }

  // // TODO: 
  // implicit def tailRecContinuationAwaitDsl[Value](implicit
  //     executionContext: ExecutionContext
  // ): Dsl[Await[Value], TailRec[Unit] !! Throwable, Value]

  implicit def continuationAwaitDsl[Value](implicit
      executionContext: ExecutionContext
  ): Dsl[Await[Value], Unit !! Throwable, Value] =
    new Dsl[Await[Value], Unit !! Throwable, Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Unit !! Throwable): Unit !! Throwable =
        !!.fromTryContinuation[Unit, Value](keyword.onComplete)(handler)
    }
  extension [FA, A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< Future[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift(Await(asFA(fa))): A

}
