package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword
import Dsl.cpsApply

opaque type Suspend[Keyword] = () => Keyword
object Suspend {
  @inline def apply[Keyword]: (() => Keyword) =:= Suspend[Keyword] = summon

  given[Upstream, UpstreamValue](using upstreamIsKeyword: => AsKeyword.FromKeyword[Upstream, UpstreamValue]): AsKeyword.FromKeyword[Suspend[Upstream], UpstreamValue] with {}

  given[Keyword, Domain, Value](using Dsl.PolyCont[Keyword, Domain, Value]): Dsl.PolyCont[Suspend[Keyword], Domain, Value] with {
    def cpsApply(keyword: Suspend[Keyword], handler: Value => Domain): Domain = {
      keyword().cpsApply(handler)          
    }
  }
}