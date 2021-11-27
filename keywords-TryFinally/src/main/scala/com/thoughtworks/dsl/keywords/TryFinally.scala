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
  given [TryKeyword, FinalizerKeyword, State, Domain, Value](using
      not: util.NotGiven[Dsl.Derived[
        TryFinally[TryKeyword, FinalizerKeyword],
        State => Domain,
        Value
      ]],
      restDsl: Dsl[
        TryFinally[Shift[Domain, Value], Shift[Domain, Any]],
        Domain,
        Value
      ],
      blockDsl: Dsl[
        TryKeyword,
        State => Domain,
        Value
      ],
      finalizerDsl: Dsl[
        FinalizerKeyword,
        State => Domain,
        Any
      ]
  ): Dsl[
    TryFinally[TryKeyword, FinalizerKeyword],
    State => Domain,
    Value
  ] with {
    def cpsApply(
        keyword: TryFinally[TryKeyword, FinalizerKeyword],
        handler: Value => State => Domain
    ): State => Domain = { state =>
      import Typed.given
      def withState[Keyword, Value](block: Keyword)(using Dsl[Keyword, State => Domain, Value]) = Shift {
        (blockHandler: Value => Domain) =>
          block.cpsApply { (value: Value) => (state: State) =>
            blockHandler(value)
          }(state)
      }
      restDsl.cpsApply(
        TryFinally(withState(keyword.block), withState(keyword.finalizer)),
        handler(_)(state)
      )
    }
  }

  // TODO: Lift
  given [
      TryKeyword,
      FinalizerKeyword,
      DomainValue,
      Value
  ](using
      ExecutionContext,
      Dsl[TryKeyword, Future[Value], Value],
      Dsl[FinalizerKeyword, Future[Any], Any]
  ): Dsl[TryFinally[TryKeyword, FinalizerKeyword], Future[DomainValue], Value] with {
    def cpsApply(
        keyword: TryFinally[TryKeyword, FinalizerKeyword],
        handler: Value => Future[DomainValue]
    ): Future[DomainValue] = {
      catchNativeException(keyword.block)
        .recoverWith { case e: Throwable =>
          catchNativeException(keyword.finalizer).flatMap { _ =>
            Future.failed(e)
          }
        }
        .flatMap { a =>
          catchNativeException(keyword.finalizer).flatMap { _ =>
            handler(a)
          }
        }
    }
  }
}
