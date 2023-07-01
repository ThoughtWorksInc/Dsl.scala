package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.cpsApply
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls

opaque type Suspend[+Keyword] <: Dsl.Keyword.Opaque =
  Dsl.Keyword.Opaque.Of[() => Keyword]
@inline def Suspend[Keyword](using
    dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
): (() => Keyword) =:= Suspend[Keyword] =
  Dsl.Keyword.Opaque.Of
object Suspend extends Suspend.LowPriority0 {

  given [Upstream, UpstreamValue](using
      upstreamIsKeyword: => IsKeyword[Upstream, UpstreamValue]
  ): IsKeyword[Suspend[Upstream], UpstreamValue] with {}

  private[Suspend] trait LowPriority1:
    given [Keyword, Domain, Value](using
        Dsl.Searching[Keyword, Domain, Value]
    ): Dsl.Composed[Suspend[Keyword], Domain, Value] = Dsl.Composed {
      (keyword: Suspend[Keyword], handler: Value => Domain) =>
        keyword().cpsApply(handler)
    }
  private[Suspend] trait LowPriority0 extends LowPriority1:
    given [Keyword, State, Domain, Value](using
        Dsl.Searching[Keyword, State => Domain, Value]
    ): Dsl.Composed[Suspend[Keyword], State => Domain, Value] = Dsl.Composed {
      (keyword: Suspend[Keyword], handler: Value => State => Domain) =>
        Dsl.TrampolineFunction1(() => keyword().cpsApply(handler))
    }

  given [Keyword, Domain, Value](using
      Dsl.Searching[Keyword, Domain !! Throwable, Value]
  ): Dsl.Composed[Suspend[Keyword], Domain !! Throwable, Value] = Dsl.Composed {
    (keyword: Suspend[Keyword], handler: Value => (Domain !! Throwable)) =>
      Dsl.TrampolineContinuation(() => keyword().cpsApply(handler))
  }

  given [Keyword, Result, Value](using
      Dsl.Searching[Keyword, Future[Result], Value],
      ExecutionContext
  ): Dsl.Composed[Suspend[Keyword], Future[Result], Value] = Dsl.Composed {
    (keyword: Suspend[Keyword], handler: Value => Future[Result]) =>
      Future.delegate(keyword().cpsApply(handler))
  }

  given [Keyword, Result, Value](using
      Dsl.Searching[Keyword, TailRec[Result], Value]
  ): Dsl.Composed[Suspend[Keyword], TailRec[Result], Value] = Dsl.Composed {
    (keyword: Suspend[Keyword], handler: Value => TailRec[Result]) =>
      TailCalls.tailcall { keyword().cpsApply(handler) }
  }

}
