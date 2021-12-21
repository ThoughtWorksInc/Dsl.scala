package com.thoughtworks.dsl
package keywords
import Dsl.!!
import Dsl.cpsApply
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal

case class TryFinally[+TryKeyword, +FinalizerKeyword](
    block: TryKeyword,
    finalizer: FinalizerKeyword
) extends Dsl.Keyword.Trait

object TryFinally {

  given [Value, OuterDomain, BlockKeyword, BlockDomain, FinalizerKeyword, FinalizerDomain](using
      dslTryFinally: Dsl.TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
      blockDsl: Dsl[BlockKeyword, BlockDomain, Value],
      finalizerDsl: Dsl[FinalizerKeyword, FinalizerDomain, Unit]
  ): Dsl.Composed[TryFinally[BlockKeyword, FinalizerKeyword], OuterDomain, Value] = {
    case (TryFinally(blockKeyword, finalizerKeyword), handler) =>
      dslTryFinally.tryFinally(
        blockDsl.cpsApply(blockKeyword, _),
        finalizerDsl.cpsApply(finalizerKeyword, _),
        handler
      )
  }

}
