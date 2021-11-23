package com.thoughtworks.dsl
package keywords

import Dsl.IsKeyword


opaque type Pure[Value] = Value
object Pure {
  given[Domain, Value]: Dsl[Pure[Value], Domain, Value] {
    def cpsApply(keyword: Pure[Value], handler: Value => Domain): Domain = {
      handler(keyword)
    }
  }
  given[PureValue]: IsKeyword[Pure[PureValue], PureValue]
  @inline def cast[Value]: Value =:= Pure[Value] = summon[Value =:= Pure[Value]]
  def apply[Value](value: Value): Pure[Value] = value
}