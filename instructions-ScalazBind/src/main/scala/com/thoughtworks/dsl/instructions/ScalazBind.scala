package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Bind, Monad, MonadError, MonadTrans}

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class ScalazBind[F[_], A](fa: F[A]) extends AnyVal with Instruction[ScalazBind[F, A], A]

object ScalazBind {

  implicit final class ScalazMonadErrorCatchOps[F[_], A](catcher: Catcher[F[A]])(
      implicit monadError: MonadError[F, Throwable]) {
    def cpsCatch(continuation: (F[A] => F[A]) => F[A]): F[A] = {
      try {
        continuation { fa: F[A] =>
          monadError.handleError(fa) { e =>
            catcher.applyOrElse(e, monadError.raiseError)
          }
        }
      } catch {
        case NonFatal(e) =>
          catcher.applyOrElse(e, monadError.raiseError)
      }
    }
  }

  implicit def implicitScalazBind[F[_], A](fa: F[A]): ScalazBind[F, A] = ScalazBind(fa)

  implicit def scalazMonadTransformerDsl1[F[_[_], _], H[_], G[_], A, B](
      implicit monadTrans: MonadTrans[F],
      rest: ScalazBindTransformerDsl[H, G, A, B]): ScalazBindTransformerDsl[H, F[G, ?], A, B] =
    new ScalazBindTransformerDsl[H, F[G, ?], A, B] {

      def monad: Monad[F[G, ?]] = monadTrans(rest.monad)

      def lift(fa: H[A]): F[G, A] = monadTrans.liftM(rest.lift(fa))(rest.monad)

      def interpret(instruction: ScalazBind[H, A], handler: A => F[G, B]): F[G, B] = {
        monad.bind(lift(instruction.fa))(handler)
      }
    }

  implicit def scalazMonadTransformerDsl0[F[_[_], _], G[_], A, B](
      implicit monadTrans: MonadTrans[F],
      monad0: Monad[G]): ScalazBindTransformerDsl[G, F[G, ?], A, B] =
    new ScalazBindTransformerDsl[G, F[G, ?], A, B] {
      def monad = monadTrans(monad0)

      def lift(fa: G[A]): F[G, A] = monadTrans.liftM(fa)

      def interpret(instruction: ScalazBind[G, A], handler: A => F[G, B]): F[G, B] = {
        monad.bind(lift(instruction.fa))(handler)
      }
    }

  implicit def scalazBindDsl[F[_], A, B](implicit bind: Bind[F]): Dsl[ScalazBind[F, A], F[B], A] =
    new Dsl[ScalazBind[F, A], F[B], A] {
      def interpret(instruction: ScalazBind[F, A], handler: A => F[B]): F[B] = {
        bind.bind(instruction.fa)(handler)
      }
    }

  abstract class ScalazBindTransformerDsl[F[_], G[_], A, B] extends Dsl[ScalazBind[F, A], G[B], A] {
    def monad: Monad[G]

    def lift(fa: F[A]): G[A]

    // We cannot define `interpret` here due to [[https://github.com/scala/bug/issues/10595]].
    /*
    def interpret(instruction: ScalazBind[F, A], handler: A => G[B]): G[B] = {
      monad.bind(lift(instruction.fa))(handler)
    }
   */
  }

}
