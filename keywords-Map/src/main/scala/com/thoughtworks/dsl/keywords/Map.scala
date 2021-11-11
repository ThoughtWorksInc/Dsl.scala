package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl

final case class Map[UpstreamKeyword, UpstreamValue, Value](upstream: UpstreamKeyword, mapper: UpstreamValue => Value)
    extends Dsl.Keyword[Map[UpstreamKeyword, UpstreamValue, Value], Value]

object Map {
  implicit def mapDsl[UpstreamKeyword, UpstreamValue, Domain, Value](implicit
      upstreamDsl: Dsl[UpstreamKeyword, Domain, UpstreamValue]
  ): Dsl[Map[UpstreamKeyword, UpstreamValue, Value], Domain, Value] =
    new Dsl[Map[UpstreamKeyword, UpstreamValue, Value], Domain, Value] {
      def cpsApply(keyword: Map[UpstreamKeyword, UpstreamValue, Value], handler: Value => Domain) = {
        val Map(upstream, mapper) = keyword
        upstreamDsl.cpsApply(upstream, mapper.andThen(handler))
      }
    }
}
