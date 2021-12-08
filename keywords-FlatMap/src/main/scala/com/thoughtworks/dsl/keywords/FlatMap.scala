package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl
import Dsl.AsKeyword
import Dsl.Typed
import scala.util.NotGiven

final case class FlatMap[Upstream, UpstreamValue, Mapped](
    upstream: Upstream,
    flatMapper: UpstreamValue => Mapped
)

object FlatMap {
  given [Upstream, UpstreamValue, Mapped, MappedValue](using
      AsKeyword.FromKeyword[Mapped, MappedValue]
  ): AsKeyword.FromKeyword[FlatMap[Upstream, UpstreamValue, Mapped], MappedValue] with {}

  given [
      Upstream,
      UpstreamValue,
      Mapped,
      MappedValue,
      Domain
  ](using
      upstreamDsl: Dsl.PolyCont[Upstream, Domain, UpstreamValue],
      nestedDsl: Dsl.PolyCont[Mapped, Domain, MappedValue]
  ): Dsl.PolyCont[FlatMap[Upstream, UpstreamValue, Mapped], Domain, MappedValue] with {
    def cpsApply(
        keyword: FlatMap[Upstream, UpstreamValue, Mapped],
        handler: MappedValue => Domain
    ): Domain = {
      val FlatMap(upstream, flatMapper) = keyword
      upstreamDsl.cpsApply(
        upstream,
        { upstreamValue =>
          nestedDsl.cpsApply(flatMapper(upstreamValue), handler)
        }
      )
    }
  }
}
