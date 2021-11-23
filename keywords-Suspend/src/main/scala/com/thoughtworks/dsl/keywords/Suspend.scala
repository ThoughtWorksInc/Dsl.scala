package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed
import Dsl.cpsApply

opaque type Suspend[Keyword] = () => Keyword
object Suspend {
  @inline def cast[Keyword]: (() => Keyword) =:= Suspend[Keyword] = implicitly
  def apply[Keyword, Value](keywordFunction: () => Keyword): Suspend[Keyword] = keywordFunction

  given[Upstream, UpstreamValue](given upstreamIsKeyword: => IsKeyword[Upstream, UpstreamValue]): IsKeyword[Suspend[Upstream], UpstreamValue]

  given[Keyword, Domain, Value](given Dsl[Keyword, Domain, Value]): Dsl[Suspend[Keyword], Domain, Value] {
    def cpsApply(keyword: Suspend[Keyword], handler: Value => Domain): Domain = {
      keyword().cpsApply(handler)          
    }
  }
}