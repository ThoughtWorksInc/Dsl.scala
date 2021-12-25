package com.thoughtworks.dsl
package keywords

import Dsl.IsKeyword

opaque type Pure[+Value] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[Value]
object Pure {
  given [Domain, Value](using
      shiftDsl: Dsl.Searching[Shift[Domain, Value], Domain, Value]
  ): Dsl.Original[Pure[Value], Domain, Value] = Dsl.Original {
    (keyword: Pure[Value], handler: Value => Domain) =>
      shiftDsl(Shift(_(keyword)), handler)
  }

  given [PureValue]: IsKeyword[Pure[PureValue], PureValue] with {}
  @inline def apply[Value]: Value =:= Pure[Value] = Dsl.Keyword.Opaque.Of.apply
}
