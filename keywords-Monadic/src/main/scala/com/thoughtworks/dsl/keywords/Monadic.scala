package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.Keyword
import scala.language.higherKinds
import scala.language.implicitConversions

final case class Monadic[F[*], A](fa: F[A]) extends Keyword[Monadic[F, A], A]

object Monadic {
  implicit def implicitFlatMap[F[*], A](fa: F[A]): Monadic[F, A] = Monadic(fa)
}
