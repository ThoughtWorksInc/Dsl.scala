package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.Keyword
import scala.language.higherKinds
import scala.language.implicitConversions

/** A keyword for extracting monadic value from the monadic expression [[fa]].
  *
  * @see [[com.thoughtworks.dsl.domains.cats]] for using this [[Monadic]] keyword with [[cats.Monad]].
  * @see [[com.thoughtworks.dsl.domains.scalaz]] for using this [[Monadic]] keyword with [[scalaz.Monad]].
  * @todo [[Monadic]] should be a [[scala.AnyVal]] after [[https://github.com/scala/bug/issues/10595]] is resolved.
  */
final case class Monadic[F[_], A](fa: F[A]) extends Keyword[Monadic[F, A], A]

object Monadic {
  implicit def implicitMonadic[F[_], A](fa: F[A]): Monadic[F, A] = Monadic(fa)
}
