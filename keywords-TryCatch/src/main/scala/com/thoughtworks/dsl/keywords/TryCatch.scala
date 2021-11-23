package com.thoughtworks.dsl
package keywords
import Dsl.Typed
import Dsl.!!
import Dsl.cpsApply
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal

case class TryCatch[TryKeyword, CaseSet](
    block: TryKeyword,
    cases: Catcher[CaseSet]
)
object TryCatch {
  private def catchNativeException[Keyword, Value](
      keyword: Keyword
  )(using dsl: Dsl[Keyword, Future[Value], Value]): Future[Value] = {
    try {
      keyword.cpsApply(Future.successful)
    } catch {
      case NonFatal(e) =>
        Future.failed(e)
    }
  }

  given [TryKeyword, TryValue <: Value, CaseSet, DomainValue, CaseValue <: Value, Value](using
      executionContext: ExecutionContext,
      tryDsl: Dsl[TryKeyword, Future[TryValue], TryValue],
      caseDsl: Dsl[CaseSet, Future[CaseValue], CaseValue]
  ): Dsl[
    TryCatch[TryKeyword, CaseSet],
    Future[DomainValue],
    Value
  ] with {
    def cpsApply(keyword: TryCatch[TryKeyword, CaseSet], handler: Value => Future[DomainValue]): Future[DomainValue] = {
      catchNativeException(keyword.block)
        .recoverWith { case e: Throwable =>
          def recover(): Future[Value] = {
            (try {
              keyword.cases.lift(e)
            } catch {
              case NonFatal(extractorException) =>
                return Future.failed(extractorException)
            }) match {
              case None =>
                Future.failed(e)
              case Some(recovered) =>
                catchNativeException(recovered)
            }
          }
          recover()
        }
        .flatMap(handler)
    }
  }
}
