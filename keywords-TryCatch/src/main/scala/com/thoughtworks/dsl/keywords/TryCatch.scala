package com.thoughtworks.dsl
package keywords
import Dsl.!!
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal

case class TryCatch[+BlockKeyword, +CaseKeyword](
    block: () => BlockKeyword,
    cases: Catcher[CaseKeyword]
) extends Dsl.Keyword.Trait
object TryCatch extends TryCatch.LegacyInstances {

  trait LegacyInstances:
    @deprecated(
      "Dsl.TryCatch / Dsl.TryFinally / Dsl.TryCatchFinally will be removed",
      "2.0.0"
    )
    given [Value, OuterDomain, BlockKeyword, BlockDomain, CaseKeyword](using
        dslTryCatch: Dsl.TryCatch[Value, OuterDomain, BlockDomain],
        blockDsl: Dsl.Searching[BlockKeyword, BlockDomain, Value],
        caseDsl: Dsl.Searching[CaseKeyword, BlockDomain, Value]
    ): Dsl.Composed[TryCatch[BlockKeyword, CaseKeyword], OuterDomain, Value] =
      Dsl.Composed { case (TryCatch(blockKeyword, cases), handler) =>
        dslTryCatch.tryCatch(
          // TODO: Use Suspend to catch the exception
          blockDsl(blockKeyword(), _),
          cases.andThen { caseKeyword => caseDsl(caseKeyword, _) },
          handler
        )
      }

}
