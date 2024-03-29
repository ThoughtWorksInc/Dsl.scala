package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.cpsApply

final case class If[+ConditionKeyword, +ThenKeyword, +ElseKeyword](
    cond: ConditionKeyword,
    thenp: () => ThenKeyword,
    elsep: () => ElseKeyword
) extends Dsl.Keyword.Trait

object If {
  given [ConditionKeyword, ThenKeyword, ThenValue, ElseKeyword, ElseValue](using
      IsKeyword[ThenKeyword, ThenValue],
      IsKeyword[ElseKeyword, ElseValue]
  ): IsKeyword[
    If[ConditionKeyword, ThenKeyword, ElseKeyword],
    ThenValue | ElseValue
  ] with {}
  given [ConditionKeyword, ThenKeyword, ElseKeyword, Domain, Value](using
      Dsl.Searching[ConditionKeyword, Domain, Boolean],
      Dsl.Searching[ThenKeyword, Domain, Value],
      Dsl.Searching[ElseKeyword, Domain, Value]
  ): Dsl.Composed[If[
    ConditionKeyword,
    ThenKeyword,
    ElseKeyword
  ], Domain, Value] = Dsl.Composed {
    (
        keyword: If[ConditionKeyword, ThenKeyword, ElseKeyword],
        handler: Value => Domain
    ) =>
      keyword.cond.cpsApply {
        case true =>
          keyword.thenp().cpsApply(handler)
        case false =>
          keyword.elsep().cpsApply(handler)
      }
  }
}
