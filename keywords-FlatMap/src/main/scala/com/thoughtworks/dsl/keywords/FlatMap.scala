package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed

final case class FlatMap[Upstream, UpstreamValue, Mapped](
  upstream: Upstream,
  flatMapper: UpstreamValue => Mapped
)

object FlatMap {
  given[Upstream, UpstreamValue, Mapped, MappedValue](given IsKeyword[Mapped, MappedValue]): IsKeyword[FlatMap[Upstream, UpstreamValue, Mapped], MappedValue]

  given[
    Upstream, UpstreamValue, Mapped, MappedValue,
    Domain
  ](
    given
    upstreamDsl: Dsl[Upstream, Domain, UpstreamValue],
    mappedDsl: Dsl[Mapped, Domain, MappedValue],
  ): Dsl[FlatMap[Upstream, UpstreamValue, Mapped], Domain, MappedValue] {
    def cpsApply(
      keyword: FlatMap[Upstream, UpstreamValue, Mapped],
      handler: MappedValue => Domain
    ): Domain = {
      upstreamDsl.cpsApply(keyword.upstream, { a =>
        val b = keyword.flatMapper(a)
        mappedDsl.cpsApply(b, handler)
      })
    }
  }
}
