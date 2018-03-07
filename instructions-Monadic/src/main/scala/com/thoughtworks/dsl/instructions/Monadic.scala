package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl.Instruction
import scala.language.higherKinds
import scala.language.implicitConversions

final case class Monadic[F[*], A](fa: F[A]) extends Instruction[Monadic[F, A], A]

object Monadic {
  implicit def implicitFlatMap[F[*], A](fa: F[A]): Monadic[F, A] = Monadic(fa)
}
