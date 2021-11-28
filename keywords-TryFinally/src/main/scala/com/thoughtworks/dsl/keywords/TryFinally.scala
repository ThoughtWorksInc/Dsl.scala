package com.thoughtworks.dsl
package keywords
import Dsl.Typed
import Dsl.!!
import Dsl.cpsApply
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal

case class TryFinally[TryKeyword, FinalizerKeyword](
    block: TryKeyword,
    finalizer: FinalizerKeyword
)

object TryFinally {

  given [Value, OuterDomain, BlockKeyword, BlockDomain, FinalizerKeyword, FinalizerDomain](using
      not: util.NotGiven[Dsl.Derived[TryFinally[BlockKeyword, FinalizerKeyword], OuterDomain, Value]],
      dslTryFinally: Dsl.TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
      blockDsl: Dsl[BlockKeyword, BlockDomain, Value],
      finalizerDsl: Dsl[FinalizerKeyword, FinalizerDomain, Unit]
  ): Dsl[TryFinally[BlockKeyword, FinalizerKeyword], OuterDomain, Value] = {
    case (TryFinally(blockKeyword, finalizerKeyword), handler) =>
      dslTryFinally.tryFinally(
        blockDsl.cpsApply(blockKeyword, _),
        finalizerDsl.cpsApply(finalizerKeyword, _),
        handler
      )
  }

}
