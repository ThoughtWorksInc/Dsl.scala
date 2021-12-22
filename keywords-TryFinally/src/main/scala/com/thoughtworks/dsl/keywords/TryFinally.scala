package com.thoughtworks.dsl
package keywords
import Dsl.!!
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
      blockDsl: Dsl.Searching[BlockKeyword, BlockDomain, Value],
      finalizerDsl: Dsl.Searching[FinalizerKeyword, FinalizerDomain, Unit]
  ): Dsl.Composed[TryFinally[BlockKeyword, FinalizerKeyword], OuterDomain, Value] = Dsl.Composed {
    case (TryFinally(blockKeyword, finalizerKeyword), handler) =>
      dslTryFinally.tryFinally(
        blockDsl(blockKeyword, _),
        finalizerDsl(finalizerKeyword, _),
        handler
      )
  }

}
