package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.domains.Continuation.!!
import scalajs.js
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

private trait AwaitJS { this: Await.type =>
  given [PromiseResult]
      : Dsl.IsKeyword[Await[js.Promise[PromiseResult]], PromiseResult] with {}

  given [JsPromiseResult, That]: Dsl.Original[Await[
    js.Promise[JsPromiseResult]
  ], js.Promise[That], JsPromiseResult] =
    Await
      .liftCo[[X] =>> Dsl.Original[X, js.Promise[That], JsPromiseResult]](
        Dsl.Original(_ `then` _)
      )

  given [JsPromiseResult, That](using ExecutionContext): Dsl.Original[Await[
    js.Promise[JsPromiseResult]
  ], Future[That], JsPromiseResult] =
    Await.liftCo[[X] =>> Dsl.Original[X, Future[That], JsPromiseResult]](
      Dsl.Original { (promise, handler) =>
        promise.toFuture.flatMap(handler)
      }
    )

  extension [FA, A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< js.Promise[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift(Await(asFA(fa))): A
}
