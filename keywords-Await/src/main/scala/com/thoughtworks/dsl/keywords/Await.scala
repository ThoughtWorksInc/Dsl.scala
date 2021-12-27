package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.domains.Continuation.!!
import scala.concurrent.Await.result
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.*

/** [[Await]] is a [[Dsl.Keyword Keyword]] to extract value from a
  * [[scala.concurrent.Future]].
  *
  * This keyword is available in functions whose return types are
  * [[scala.concurrent.Future Future]], [[domains.task.Task]], or any exception
  * aware continuations as `(_ !! Throwable !! _)`.
  *
  * @example
  *   Given a [[scala.concurrent.Future Future]]:
  * {{{
  * import com.thoughtworks.dsl.macros.Reset.Default.*
  * import scala.concurrent.Future
  * val myFuture40 = Future {
  *   40
  * }
  * }}}
  *
  * You can [[Await]] the [[scala.concurrent.Future Future]] in another
  * [[scala.concurrent.Future Future]]
  *
  * {{{
  * def myFuture42 = *[Future] {
  *   !Await(myFuture40) + 2
  * }
  * }}}
  *
  * A [[scala.concurrent.Future Future]] can be converted to a
  * [[domains.task.Task]] with the help of [[Await]].
  *
  * {{{
  * import com.thoughtworks.dsl.domains.Task
  * import com.thoughtworks.dsl.keywords.Await
  * val myTask = Task {
  *   !Await(myFuture42)
  * }
  * }}}
  *
  * Then a [[domains.task.Task]] can be converted back to a
  * [[scala.concurrent.Future]] via [[domains.task.Task.toFuture]].
  *
  * {{{
  * val myAssertionTask = Task {
  *   !Shift(myTask) should be(42)
  * }
  * Task.toFuture(myAssertionTask)
  * }}}
  * @example
  *   `!Await` can be used together with `try` / `catch` / `finally`.
  * {{{
  * import scala.concurrent.Future
  * import com.thoughtworks.dsl.macros.Reset.Default.*
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
  *   Other keywords, including [[Return]] or [[Get]], can be used together with
  *   [[Await]]
  * {{{
  * import scala.concurrent.Future
  * import com.thoughtworks.dsl.keywords.{Get, Return}
  * import com.thoughtworks.dsl.macros.Reset.Default.*
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
  *       buffer.append(!Get[Char])
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
opaque type Await[+AwaitableValue] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[AwaitableValue]
object Await extends AwaitJS with Await.LowPriority0 {
  @inline def apply[AwaitableValue]: AwaitableValue =:= Await[AwaitableValue] =
    Dsl.Keyword.Opaque.Of.apply
  given [FutureResult]: IsKeyword[Await[Future[FutureResult]], FutureResult]
    with {}

  given [FutureResult, That](using
      ExecutionContext
  ): Dsl.Original[Await[Future[FutureResult]], Future[That], FutureResult] =
    Dsl.Original(_ flatMap _)

  // // TODO:
  // implicit def tailRecContinuationAwaitDsl[Value](implicit
  //     executionContext: ExecutionContext
  // ): Dsl.Original[Await[Value], TailRec[Unit] !! Throwable, Value]
  private[Await] trait LowPriority0:
    inline given [Domain, Value](using
        fenceDsl: Dsl.Searching[Fence.type, Domain, Unit],
        liftUnit: Dsl.Lift[Unit, Domain],
        executionContext: ExecutionContext,
    ): Dsl.Composed[Await[Future[Value]], Domain, Value] = Dsl.Composed {
      println(compiletime.codeOf(
        {(x: Domain) => ()}
      ))

      (keyword: Await[Future[Value]], handler: Value => Domain) =>
        keyword.onComplete {
          case Failure(e) =>
            fenceDsl(Fence, (_: Unit) => throw e)
          case Success(result) =>
            fenceDsl(Fence, (_: Unit) => handler(result))
        }
        liftUnit(())
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
