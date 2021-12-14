package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl

final case class WithFilter[UpstreamKeyword, UpstreamValue](
    upstream: UpstreamKeyword,
    condition: UpstreamValue => Boolean
) extends Dsl.Keyword.Trait

object WithFilter {
  given [UpstreamKeyword, UpstreamValue]: Dsl.AsKeyword.IsKeyword[WithFilter[
    UpstreamKeyword,
    UpstreamValue
  ], UpstreamValue] with {}

  implicit def withFilterDsl[UpstreamKeyword, Domain, UpstreamValue](implicit
      upstreamDsl: Dsl.PolyCont[UpstreamKeyword, Domain, UpstreamValue],
      continueDsl: Dsl[Continue, Domain, Nothing]
  ): Dsl.PolyCont[WithFilter[
    UpstreamKeyword,
    UpstreamValue
  ], Domain, UpstreamValue] = {
    (
        keyword: WithFilter[UpstreamKeyword, UpstreamValue],
        handler: UpstreamValue => Domain
    ) =>
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
