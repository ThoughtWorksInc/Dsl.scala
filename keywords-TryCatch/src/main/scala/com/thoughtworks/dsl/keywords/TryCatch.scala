package com.thoughtworks.dsl
package keywords
import Dsl.!!
import Dsl.cpsApply
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal

case class TryCatch[+BlockKeyword, +CaseKeyword](block: BlockKeyword, cases: Catcher[CaseKeyword]) extends Dsl.Keyword.Trait
object TryCatch {

  given [Value, OuterDomain, BlockKeyword, BlockDomain, CaseKeyword](
      using
      dslTryCatch: Dsl.TryCatch[Value, OuterDomain, BlockDomain],
      blockDsl: Dsl[BlockKeyword, BlockDomain, Value],
      caseDsl: Dsl[CaseKeyword, BlockDomain, Value],
  ): Dsl[TryCatch[BlockKeyword, CaseKeyword], OuterDomain, Value] = {
    case (TryCatch(blockKeyword, cases), handler) =>
      dslTryCatch.tryCatch(
        blockDsl.cpsApply(blockKeyword, _),
        cases.andThen { caseKeyword => caseDsl.cpsApply(caseKeyword, _) },
        handler
      )
  }

}
