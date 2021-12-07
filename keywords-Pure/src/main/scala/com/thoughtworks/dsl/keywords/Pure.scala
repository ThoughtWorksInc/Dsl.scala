package com.thoughtworks.dsl
package keywords

import Dsl.IsKeyword

opaque type Pure[+Value] = Value
object Pure {
  given [Domain, Value](using
      shiftDsl: Dsl.PolyCont[Shift[Domain, Value], Domain, Value]
  ): Dsl[Pure[Value], Domain, Value] = { (keyword: Pure[Value], handler: Value => Domain) =>
    shiftDsl.cpsApply(Shift(_(keyword)), handler)
  }

  given [PureValue]: IsKeyword[Pure[PureValue], PureValue] with {}
  @inline def cast[Value]: Value =:= Pure[Value] = summon[Value =:= Pure[Value]]
  def apply[Value](value: Value): Pure[Value] = value
}
