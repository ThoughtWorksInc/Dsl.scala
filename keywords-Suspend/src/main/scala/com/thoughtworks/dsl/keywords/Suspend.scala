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
object Suspend extends Suspend.LowPriority0 {
  @inline def apply[Keyword]: (() => Keyword) =:= Suspend[Keyword] =
    Dsl.Keyword.Opaque.Of.apply

  given [Upstream, UpstreamValue](using
      upstreamIsKeyword: => IsKeyword[Upstream, UpstreamValue]
  ): IsKeyword[Suspend[Upstream], UpstreamValue] with {}

  private[Suspend] trait LowPriority0:
    given [Keyword, Domain, Value](using
        Dsl.Searching[Keyword, Domain, Value]
    ): Dsl.Composed[Suspend[Keyword], Domain, Value] = Dsl.Composed {
      (keyword: Suspend[Keyword], handler: Value => Domain) =>
        keyword().cpsApply(handler)
    }

  given [Keyword, State, Domain, Value](using
      Dsl.Searching[Keyword, State => Domain, Value]
  ): Dsl.Composed[Suspend[Keyword], State => Domain, Value] = Dsl.Composed {
    (keyword: Suspend[Keyword], handler: Value => State => Domain) => value =>
      keyword().cpsApply(handler)(value)
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
