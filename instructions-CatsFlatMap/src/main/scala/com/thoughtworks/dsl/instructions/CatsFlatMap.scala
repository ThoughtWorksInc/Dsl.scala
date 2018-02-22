package com.thoughtworks.dsl.instructions

import cats.FlatMap
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import cats.MonadError

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class CatsFlatMap[F[_], A](fa: F[A]) extends AnyVal with Instruction[CatsFlatMap[F, A], A]

object CatsFlatMap {

  implicit final class CatsMonadErrorCatchOps[F[_], A](catcher: Catcher[F[A]])(
      implicit monadError: MonadError[F, Throwable]) {
    def cpsCatch(continuation: (F[A] => F[A]) => F[A]): F[A] = {
      try {
        continuation { fa: F[A] =>
          monadError.handleErrorWith(fa) { e =>
            catcher.applyOrElse(e, monadError.raiseError)
          }
        }
      } catch {
        case NonFatal(e) =>
          catcher.applyOrElse(e, monadError.raiseError)
      }
    }
  }

  implicit def implicitCatsFlatMap[F[_], A](fa: F[A]): CatsFlatMap[F, A] = CatsFlatMap(fa)

  implicit def scalazBindDsl[F[_], A, B](implicit flatMap: FlatMap[F]): Dsl[CatsFlatMap[F, A], F[B], A] =
    new Dsl[CatsFlatMap[F, A], F[B], A] {
      def interpret(instruction: CatsFlatMap[F, A], handler: A => F[B]): F[B] = {
        flatMap.flatMap(instruction.fa)(handler)
      }
    }

}
