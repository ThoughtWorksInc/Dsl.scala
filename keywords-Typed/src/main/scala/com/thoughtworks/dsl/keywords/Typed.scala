package com.thoughtworks.dsl
package keywords

/** A type annotated keyword */
opaque type Typed[+Keyword, +Value] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[Keyword]

@inline def Typed[Keyword, Value](using
    dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
): Keyword =:= Typed[Keyword, Value] =
  Dsl.Keyword.Opaque.Of
object Typed {
  given [Keyword, Value]: Dsl.IsKeyword[Typed[Keyword, Value], Value] with {}
  given [Keyword, Domain, Value](using
      dsl: Dsl.Searching[Keyword, Domain, Value]
  ): Dsl.Composed[Typed[Keyword, Value], Domain, Value] =
    Dsl.Composed(dsl)

}
