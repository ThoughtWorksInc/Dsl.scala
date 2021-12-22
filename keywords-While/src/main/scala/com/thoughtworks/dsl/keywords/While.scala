package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.cpsApply

case class While[
    +ConditionKeyword,
    +BodyKeyword
](
    condition: ConditionKeyword,
    body: BodyKeyword
) extends Dsl.Keyword.Trait
object While {
  given [
      ConditionKeyword,
      BodyKeyword,
      Domain
  ](using
      conditionDsl: Dsl.Searching[ConditionKeyword, Domain, Boolean],
      bodyDsl: Dsl.Searching[BodyKeyword, Domain, Any]
  ): Dsl.Composed[
    While[ConditionKeyword, BodyKeyword],
    Domain,
    Unit
  ] = Dsl.Composed {
    (keyword: While[ConditionKeyword, BodyKeyword], handler: Unit => Domain) =>
      keyword.condition.cpsApply {
        case true =>
          keyword.body.cpsApply { _ =>
            keyword.cpsApply(handler)
          }
        case false =>
          handler(())
      }
  }
}
