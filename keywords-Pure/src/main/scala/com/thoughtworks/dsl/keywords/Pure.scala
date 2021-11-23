package com.thoughtworks.dsl
package keywords

import Dsl.IsKeyword


opaque type Pure[Value] = Value
object Pure {
  given[Domain, Value]: Dsl[Pure[Value], Domain, Value] with {
    def cpsApply(keyword: Pure[Value], handler: Value => Domain): Domain = {
      handler(keyword)
    }
  }
  given[PureValue]: IsKeyword[Pure[PureValue], PureValue] with {}
  @inline def cast[Value]: Value =:= Pure[Value] = summon[Value =:= Pure[Value]]
  def apply[Value](value: Value): Pure[Value] = value
}