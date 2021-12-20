package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.domains.Continuation.!!
import scalajs.js
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal

private trait AwaitJS { this: Await.type =>
  given [PromiseResult]
      : Dsl.IsKeyword[Await[js.Promise[PromiseResult]], PromiseResult] with {}

  given [Value, That]: Dsl[Await[js.Promise[Value]], js.Promise[That], Value] =
    Await.apply.liftCo[[X] =>> Dsl[X, js.Promise[That], Value]](_ `then` _)

  extension [FA, A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< js.Promise[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift(Await(asFA(fa))): A
}
