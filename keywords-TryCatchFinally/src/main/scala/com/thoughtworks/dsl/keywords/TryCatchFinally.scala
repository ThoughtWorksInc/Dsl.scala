package com.thoughtworks.dsl
package keywords

import Dsl.IsKeyword
import scala.util.control.Exception.Catcher

case class TryCatchFinally[+BlockKeyword, +CaseKeyword, +FinalizerKeyword](
    block: () => BlockKeyword,
    cases: Catcher[CaseKeyword],
    finalizer: () => FinalizerKeyword
) extends Dsl.Keyword.Trait
object TryCatchFinally {
  given [
      Value,
      OuterDomain,
      BlockKeyword,
      BlockDomain,
      CaseKeyword,
      FinalizerKeyword,
      FinalizerDomain
  ](using
      Dsl.Searching[TryFinally[
        TryCatch[BlockKeyword, CaseKeyword],
        FinalizerKeyword
      ], OuterDomain, Value]
  ): Dsl.Composed[TryCatchFinally[
    BlockKeyword,
    CaseKeyword,
    FinalizerKeyword
  ], OuterDomain, Value] = Dsl.Composed {
    case (
          TryCatchFinally(
            block,
            catcher,
            finalizer
          ),
          handler
        ) =>
      TryFinally(() => TryCatch(block, catcher), finalizer).cpsApply(handler)
  }
}
