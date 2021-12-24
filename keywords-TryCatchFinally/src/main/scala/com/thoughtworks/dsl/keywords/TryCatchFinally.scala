package com.thoughtworks.dsl
package keywords
import Dsl.!!
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher

case class TryCatchFinally[+BlockKeyword, +CaseKeyword, +FinalizerKeyword](
    block: () => BlockKeyword,
    cases: Catcher[CaseKeyword],
    finalizer: () => FinalizerKeyword
) extends Dsl.Keyword.Trait
object TryCatchFinally extends TryCatchFinally.LegacyInstances {
  trait LegacyInstances:
    @deprecated(
      "Dsl.TryCatch / Dsl.TryFinally / Dsl.TryCatchFinally will be removed",
      "2.0.0"
    )
    given [Value, OuterDomain, BlockKeyword, BlockDomain, CaseKeyword, FinalizerKeyword, FinalizerDomain](
        using
        dslTryCatchFinally: Dsl.TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
        blockDsl: Dsl.Searching[BlockKeyword, BlockDomain, Value],
        caseDsl: Dsl.Searching[CaseKeyword, BlockDomain, Value],
        finalizerDsl: Dsl.Searching[FinalizerKeyword, FinalizerDomain, Unit]
    ): Dsl.Composed[TryCatchFinally[BlockKeyword, CaseKeyword, FinalizerKeyword], OuterDomain, Value] = Dsl.Composed {
      case (TryCatchFinally(blockKeyword, cases, finalizerKeyword), handler) =>
        dslTryCatchFinally.tryCatchFinally(
          // TODO: Use Suspend to catch the exception
          blockDsl(blockKeyword(), _),
          cases.andThen { caseKeyword => caseDsl(caseKeyword, _) },
          finalizerDsl(finalizerKeyword(), _),
          handler
        )
    }

}
