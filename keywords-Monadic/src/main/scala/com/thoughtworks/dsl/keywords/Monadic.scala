package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
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
opaque type Monadic[+FA] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[FA]

object Monadic {

  extension [FA, F[_], A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< F[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift[Monadic[FA], A](Monadic[FA](fa)): A

  @inline def apply[FA]: FA =:= Monadic[FA] = Dsl.Keyword.Opaque.Of.apply

  given [FA <: F[A], F[_], A]: IsKeyword[Monadic[FA], A] with {}
}
