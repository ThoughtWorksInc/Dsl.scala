package com.thoughtworks.dsl
package keywords
import Dsl.Typed
import Dsl.!!
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher

case class TryCatchFinally[TryKeyword, CaseSet, FinalizerKeyword](
    block: TryKeyword,
    cases: Catcher[CaseSet],
    finalizer: FinalizerKeyword
)
object TryCatchFinally {
  given [
      TryKeyword,
      CaseSet,
      FinalizerKeyword,
      Domain,
      Value
  ](using
      tryFinallyDsl: Dsl[
        TryFinally[TryCatch[TryKeyword, CaseSet], FinalizerKeyword],
        Domain,
        Value,
      ]
  ): Dsl[
    TryCatchFinally[TryKeyword, CaseSet, FinalizerKeyword],
    Domain,
    Value,
  ] with {
    def cpsApply(keyword: TryCatchFinally[TryKeyword, CaseSet, FinalizerKeyword], handler: Value => Domain): Domain = {
      import Typed.given
      tryFinallyDsl.cpsApply(
        TryFinally(
          TryCatch(keyword.block, keyword.cases),
          keyword.finalizer
        ),
        handler
      )
    }
  }
}
