package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl.IsKeyword
import scala.language.implicitConversions

opaque type NoneSafe[+A] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[Option[A]]

object NoneSafe {
  given [A]: IsKeyword[NoneSafe[A], A] with {}
  def apply[A]: Option[A] =:= NoneSafe[A] = Dsl.Keyword.Opaque.Of.apply

  implicit def noneSafeDsl[A, Domain](implicit
      continueDsl: Dsl[Return[None.type], Domain, Nothing]
  ): Dsl[NoneSafe[A], Domain, A] = {
    (keyword: Option[A], handler: A => Domain) =>
      keyword match {
        case None =>
          continueDsl.cpsApply(Return(None), identity)
        case Some(a) =>
          handler(a)
      }
  }: Dsl[Option[A], Domain, A]

  extension [FA, A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< Option[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift(NoneSafe(asFA(fa))): A

}
