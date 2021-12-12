package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import Dsl.cpsApply

final case class If[ConditionKeyword, ThenKeyword, ElseKeyword](
  cond: ConditionKeyword,
  thenp: ThenKeyword,
  elsep: ElseKeyword) extends Dsl.Keyword

object If {
  given[ConditionKeyword, ThenKeyword, ElseKeyword, Domain, Value](
    using
    Dsl.PolyCont[ConditionKeyword, Domain, Boolean],
    Dsl.PolyCont[ThenKeyword, Domain, Value],
    Dsl.PolyCont[ElseKeyword, Domain, Value],
  ): Dsl.PolyCont[If[ConditionKeyword, ThenKeyword, ElseKeyword], Domain, Value] with {
    def cpsApply(keyword: If[ConditionKeyword, ThenKeyword, ElseKeyword], handler: Value => Domain): Domain = {
      keyword.cond.cpsApply{
        case true =>
          keyword.thenp.cpsApply(handler)
        case false =>
          keyword.elsep.cpsApply(handler)
      }
    }
  }
}