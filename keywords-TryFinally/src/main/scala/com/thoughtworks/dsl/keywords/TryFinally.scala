package com.thoughtworks.dsl
package keywords
import Dsl.!!
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal

case class TryFinally[+TryKeyword, +FinalizerKeyword](
    block: () => TryKeyword,
    finalizer: () => FinalizerKeyword
) extends Dsl.Keyword.Trait

object TryFinally extends TryFinally.LegacyInstances {
  trait LegacyInstances:
    @deprecated(
      "Dsl.TryCatch / Dsl.TryFinally / Dsl.TryCatchFinally will be removed",
      "2.0.0"
    )
    given [Value, OuterDomain, BlockKeyword, BlockDomain, FinalizerKeyword, FinalizerDomain](using
        dslTryFinally: Dsl.TryFinally[Value, OuterDomain, BlockDomain, FinalizerDomain],
        blockDsl: Dsl.Searching[BlockKeyword, BlockDomain, Value],
        finalizerDsl: Dsl.Searching[FinalizerKeyword, FinalizerDomain, Unit]
    ): Dsl.Composed[TryFinally[BlockKeyword, FinalizerKeyword], OuterDomain, Value] = Dsl.Composed {
      case (TryFinally(blockKeyword, finalizerKeyword), handler) =>
        dslTryFinally.tryFinally(
          // TODO: Use Suspend to catch the exception
          blockDsl(blockKeyword(), _),
          finalizerDsl(finalizerKeyword(), _),
          handler
        )
    }

}
