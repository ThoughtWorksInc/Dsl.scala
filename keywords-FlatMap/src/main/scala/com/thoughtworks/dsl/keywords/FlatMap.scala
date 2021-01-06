package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl

final case class FlatMap[UpstreamKeyword, UpstreamValue, NestedKeyword, NestedValue](
    upstream: UpstreamKeyword,
    flatMapper: UpstreamValue => NestedKeyword
) extends Dsl.Keyword[FlatMap[UpstreamKeyword, UpstreamValue, NestedKeyword, NestedValue], NestedValue]

object FlatMap {
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
