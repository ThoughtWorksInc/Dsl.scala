package com.thoughtworks.dsl.instructions

import cats.FlatMap
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
final case class CatsFlatMap[F[_], A](fa: F[A]) extends AnyVal with Instruction[CatsFlatMap[F, A], A]

object CatsFlatMap {

  implicit def scalazBindDsl[F[_], A, B](implicit flatMap: FlatMap[F]): Dsl[CatsFlatMap[F, A], F[B], A] =
    new Dsl[CatsFlatMap[F, A], F[B], A] {
      def interpret(instruction: CatsFlatMap[F, A], handler: A => F[B]): F[B] = {
        flatMap.flatMap(instruction.fa)(handler)
      }
    }

}
