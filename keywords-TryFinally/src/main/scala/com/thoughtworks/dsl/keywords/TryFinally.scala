package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal
import scala.util.Success
import scala.util.Failure
import scala.util.Try

case class TryFinally[+TryKeyword, +FinalizerKeyword](
    block: () => TryKeyword,
    finalizer: () => FinalizerKeyword
) extends Dsl.Keyword.Trait

object TryFinally {
  type DslComposer[OuterDomain, Value, BlockDomain] =
    TryCatch.DslComposer[OuterDomain, Try[Value], BlockDomain]

  given [
      Value,
      OuterDomain,
      BlockKeyword,
      BlockDomain,
      FinalizerKeyword
  ](using
      DslComposer[OuterDomain, Value, BlockDomain],
      Dsl.Searching[BlockKeyword, BlockDomain, Value],
      Dsl.Searching[FinalizerKeyword, OuterDomain, Unit]
  ): Dsl.Composed[TryFinally[
    BlockKeyword,
    FinalizerKeyword
  ], OuterDomain, Value] = Dsl.Composed {
    case (TryFinally(blockKeyword, finalizerKeyword), handler) =>
      val transformedAst = FlatMap(
        TryCatch(
          () =>
            FlatMap(
              blockKeyword(),
              (successValue: Value) => Pure(Success(successValue))
            ),
          { case NonFatal(e) =>
            Pure(Failure[Value](e))
          }
        ),
        (result: Try[Value]) =>
          FlatMap(finalizerKeyword(), (_: Unit) => Pure(result.get))
      )
      summon[Dsl.Searching[transformedAst.type, OuterDomain, Value]](
        transformedAst,
        handler
      )
  }

}
