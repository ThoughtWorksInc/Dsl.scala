package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl
import Dsl.IsKeyword
import Dsl.Typed

final case class FlatMap[Upstream, UpstreamValue, Mapped](
    upstream: Upstream,
    flatMapper: UpstreamValue => Mapped
)

object FlatMap {
  given [Upstream, UpstreamValue, Mapped, MappedValue](using
      IsKeyword[Mapped, MappedValue]
  ): IsKeyword[FlatMap[Upstream, UpstreamValue, Mapped], MappedValue] with {}

  given [
      Upstream,
      UpstreamValue,
      Mapped,
      MappedValue,
      Domain
  ](using
      upstreamDsl: Dsl[Upstream, Domain, UpstreamValue],
      mappedDsl: Dsl[Mapped, Domain, MappedValue]
  ): Dsl[FlatMap[Upstream, UpstreamValue, Mapped], Domain, MappedValue] with {
    def cpsApply(
        keyword: FlatMap[Upstream, UpstreamValue, Mapped],
        handler: MappedValue => Domain
    ): Domain = {
      upstreamDsl.cpsApply(
        keyword.upstream,
        { a =>
          val b = keyword.flatMapper(a)
          mappedDsl.cpsApply(b, handler)
        }
      )
    }
  }

  implicit def flatMapDsl[UpstreamKeyword, UpstreamValue, Domain, NestedKeyword, NestedValue](implicit
      upstreamDsl: Dsl[UpstreamKeyword, Domain, UpstreamValue],
      nestedDsl: Dsl[NestedKeyword, Domain, NestedValue]
  ): Dsl[FlatMap[UpstreamKeyword, UpstreamValue, NestedKeyword, NestedValue], Domain, NestedValue] =
    new Dsl[FlatMap[UpstreamKeyword, UpstreamValue, NestedKeyword, NestedValue], Domain, NestedValue] {
      def cpsApply(
          keyword: FlatMap[UpstreamKeyword, UpstreamValue, NestedKeyword, NestedValue],
          handler: NestedValue => Domain
      ) = {
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
