package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl.AsKeyword
import scala.language.implicitConversions

final case class NoneSafe[A](option: Option[A]) extends AnyVal with Dsl.Keyword.Trait

object NoneSafe {
  given [A]: AsKeyword.IsKeyword[NoneSafe[A], A] with {}

  implicit def noneSafeDsl[A, Domain](implicit
      continueDsl: Dsl[Return[None.type], Domain, Nothing]
  ): Dsl[NoneSafe[A], Domain, A] =
    new Dsl[NoneSafe[A], Domain, A] {
      def cpsApply(keyword: NoneSafe[A], handler: A => Domain): Domain = {
        keyword.option match {
          case None =>
            continueDsl.cpsApply(Return(None), identity)
          case Some(a) =>
            handler(a)
        }
      }
    }

  extension [FA, A](inline fa: FA)(using
      inline notKeyword: util.NotGiven[
        FA <:< Dsl.Keyword
      ],
      inline asFA: FA <:< Option[A]
  )
    transparent inline def unary_! : A =
      Dsl.shift(NoneSafe(asFA(fa))): A

}
