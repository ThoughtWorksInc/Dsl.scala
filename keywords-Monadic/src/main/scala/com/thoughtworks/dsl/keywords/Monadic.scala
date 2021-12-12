package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import scala.language.higherKinds
import scala.language.implicitConversions

/** A keyword for extracting monadic value from the monadic expression [[fa]].
  *
  * @see
  *   [[com.thoughtworks.dsl.domains.cats]] for using this [[Monadic]] keyword with [[cats.Monad]].
  * @see
  *   [[com.thoughtworks.dsl.domains.scalaz]] for using this [[Monadic]] keyword with [[scalaz.Monad]].
  * @todo
  *   [[Monadic]] should be a [[scala.AnyVal]] after [[https://github.com/scala/bug/issues/10595]] is resolved.
  */
opaque type Monadic[Functor[_], Value] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[Functor[Value]]

object Monadic {

  extension [FA, F[_], A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< F[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift[Monadic[F, A], A](Monadic[F, A](asFA(fa))): A

  @inline def apply[Functor[_], Value]: Functor[Value] =:= Monadic[Functor, Value] = Dsl.Keyword.Opaque.Of.apply

  given [Functor[_], Value]: AsKeyword.IsKeyword[Monadic[Functor, Value], Value] with {}
}
