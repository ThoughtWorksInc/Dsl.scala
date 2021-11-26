package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl

final case class WithFilter[UpstreamKeyword, UpstreamValue](
    upstream: UpstreamKeyword,
    condition: UpstreamValue => Boolean
) extends Dsl.Keyword[WithFilter[UpstreamKeyword, UpstreamValue], UpstreamValue]

object WithFilter {
  implicit def withFilterDsl[UpstreamKeyword, Domain, UpstreamValue](implicit
      upstreamDsl: Dsl[UpstreamKeyword, Domain, UpstreamValue],
      continueDsl: Dsl[Continue, Domain, Nothing]
  ): Dsl[WithFilter[UpstreamKeyword, UpstreamValue], Domain, UpstreamValue] =
    new Dsl[WithFilter[UpstreamKeyword, UpstreamValue], Domain, UpstreamValue] {
      def cpsApply(keyword: WithFilter[UpstreamKeyword, UpstreamValue], handler: UpstreamValue => Domain) = {
        val WithFilter(upstream, condition) = keyword
        upstreamDsl.cpsApply(
          upstream,
          { upstreamValue =>
            if (condition(upstreamValue)) {
              handler(upstreamValue)
            } else {
              continueDsl.cpsApply(Continue, identity)
            }
          }
        )
      }
    }
}
