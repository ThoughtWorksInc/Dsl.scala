package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl.IsKeyword
import scala.language.implicitConversions

opaque type NoneSafe[+A] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[Option[A]]

def NoneSafe[A](using
    dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
): Option[A] =:= NoneSafe[A] = Dsl.Keyword.Opaque.Of
object NoneSafe {
  given [A]: IsKeyword[NoneSafe[A], A] with {}

  given [A, Domain](using
      continueDsl: Dsl.Searching[Return[None.type], Domain, Nothing]
  ): Dsl.Original[NoneSafe[A], Domain, A] = Dsl.Original[Option[A], Domain, A] {
    (keyword: Option[A], handler: A => Domain) =>
      keyword match {
        case None =>
          continueDsl(Return(None), identity)
        case Some(a) =>
          handler(a)
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
