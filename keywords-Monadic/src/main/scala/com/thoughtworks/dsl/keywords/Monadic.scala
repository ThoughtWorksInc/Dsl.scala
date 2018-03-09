package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.Keyword
import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * @todo [[Monadic]] should be a [[scala.AnyVal]] after [[https://github.com/scala/bug/issues/10595]] is resolved.
  */
final case class Monadic[F[*], A](fa: F[A]) extends Keyword[Monadic[F, A], A]

object Monadic {
  implicit def implicitFlatMap[F[*], A](fa: F[A]): Monadic[F, A] = Monadic(fa)
}
