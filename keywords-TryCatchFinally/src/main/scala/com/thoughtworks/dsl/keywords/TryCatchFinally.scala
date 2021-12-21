package com.thoughtworks.dsl
package keywords
import Dsl.!!
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher

case class TryCatchFinally[+BlockKeyword, +CaseKeyword, +FinalizerKeyword](
    block: BlockKeyword,
    cases: Catcher[CaseKeyword],
    finalizer: FinalizerKeyword
) extends Dsl.Keyword.Trait
object TryCatchFinally {

  given [Value, OuterDomain, BlockKeyword, BlockDomain, CaseKeyword, FinalizerKeyword, FinalizerDomain](
      using
      dslTryCatchFinally: Dsl.TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
      blockDsl: Dsl[BlockKeyword, BlockDomain, Value],
      caseDsl: Dsl[CaseKeyword, BlockDomain, Value],
      finalizerDsl: Dsl[FinalizerKeyword, FinalizerDomain, Unit]
  ): Dsl[TryCatchFinally[BlockKeyword, CaseKeyword, FinalizerKeyword], OuterDomain, Value] = {
    case (TryCatchFinally(blockKeyword, cases, finalizerKeyword), handler) =>
      dslTryCatchFinally.tryCatchFinally(
        blockDsl.cpsApply(blockKeyword, _),
        cases.andThen { caseKeyword => caseDsl.cpsApply(caseKeyword, _) },
        finalizerDsl.cpsApply(finalizerKeyword, _),
        handler
      )
  }

}
