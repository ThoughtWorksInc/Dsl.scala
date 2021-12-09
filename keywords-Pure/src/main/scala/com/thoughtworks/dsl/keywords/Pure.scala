package com.thoughtworks.dsl
package keywords

import Dsl.AsKeyword

opaque type Pure[+Value] = Value
object Pure {
  given [Domain, Value](using
      shiftDsl: Dsl.PolyCont[Shift[Domain, Value], Domain, Value]
  ): Dsl[Pure[Value], Domain, Value] = { (keyword: Pure[Value], handler: Value => Domain) =>
    shiftDsl.cpsApply(Shift(_(keyword)), handler)
  }

  given [PureValue]: AsKeyword.IsKeyword[Pure[PureValue], PureValue] with {}
  @inline def apply[Value]: Value =:= Pure[Value] = summon[Value =:= Pure[Value]]
}
