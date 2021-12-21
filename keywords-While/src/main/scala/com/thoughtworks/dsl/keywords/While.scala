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
      conditionDsl: Dsl[ConditionKeyword, Domain, Boolean],
      bodyDsl: Dsl[BodyKeyword, Domain, Any]
  ): Dsl.Composed[
    While[ConditionKeyword, BodyKeyword],
    Domain,
    Unit
  ] with {
    def cpsApply(
        keyword: While[ConditionKeyword, BodyKeyword],
        handler: Unit => Domain
    ): Domain = {
      keyword.condition.cpsApply {
        case true =>
          keyword.body.cpsApply { _ =>
            cpsApply(keyword, handler)
          }
        case false =>
          handler(())
      }
    }

  }
}
