package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed
import Dsl.cpsApply

opaque type Suspend[Keyword] = () => Keyword
object Suspend {
  @inline def cast[Keyword]: (() => Keyword) =:= Suspend[Keyword] = implicitly
  def apply[Keyword, Value](keywordFunction: () => Keyword): Suspend[Keyword] = keywordFunction

  given[Upstream, UpstreamValue](using upstreamIsKeyword: => IsKeyword[Upstream, UpstreamValue]): IsKeyword[Suspend[Upstream], UpstreamValue] with {}

  given[Keyword, Domain, Value](using util.NotGiven[Dsl.Derived[Suspend[Keyword], Domain, Value]], Dsl[Keyword, Domain, Value]): Dsl[Suspend[Keyword], Domain, Value] with {
    def cpsApply(keyword: Suspend[Keyword], handler: Value => Domain): Domain = {
      keyword().cpsApply(handler)          
    }
  }
}