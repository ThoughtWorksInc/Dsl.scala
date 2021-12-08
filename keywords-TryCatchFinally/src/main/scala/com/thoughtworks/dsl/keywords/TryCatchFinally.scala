package com.thoughtworks.dsl
package keywords
import Dsl.Typed
import Dsl.!!
import Dsl.AsKeyword
import scala.util.control.Exception.Catcher

case class TryCatchFinally[BlockKeyword, CaseKeyword, FinalizerKeyword](
    block: BlockKeyword,
    cases: Catcher[CaseKeyword],
    finalizer: FinalizerKeyword
)
object TryCatchFinally {

  given [Value, OuterDomain, BlockKeyword, BlockDomain, CaseKeyword, FinalizerKeyword, FinalizerDomain](
      using
      dslTryCatchFinally: Dsl.TryCatchFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
      blockDsl: Dsl.PolyCont[BlockKeyword, BlockDomain, Value],
      caseDsl: Dsl.PolyCont[CaseKeyword, BlockDomain, Value],
      finalizerDsl: Dsl.PolyCont[FinalizerKeyword, FinalizerDomain, Unit]
  ): Dsl.PolyCont[TryCatchFinally[BlockKeyword, CaseKeyword, FinalizerKeyword], OuterDomain, Value] = {
    case (TryCatchFinally(blockKeyword, cases, finalizerKeyword), handler) =>
      dslTryCatchFinally.tryCatchFinally(
        blockDsl.cpsApply(blockKeyword, _),
        cases.andThen { caseKeyword => caseDsl.cpsApply(caseKeyword, _) },
        finalizerDsl.cpsApply(finalizerKeyword, _),
        handler
      )
  }

}
