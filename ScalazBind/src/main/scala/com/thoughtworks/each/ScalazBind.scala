package com.thoughtworks.each
import com.thoughtworks.each.ScalazBind.ScalazBindTransformerCpsApply

import scalaz.{Bind, Monad, MonadTrans}
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
final case class ScalazBind[F[_], A](fa: F[A])
    extends /* AnyVal with */ Continuation.InstructionOps[ScalazBind[F, A], A] {
  protected def self = this
}

object ScalazBind {

  implicit def scalazMonadTransformerCpsApply1[F[_[_], _], H[_], G[_], A, B](
      implicit monadTrans: MonadTrans[F],
      rest: ScalazBindTransformerCpsApply[H, G, A, B]): ScalazBindTransformerCpsApply[H, F[G, ?], A, B] =
    new ScalazBindTransformerCpsApply[H, F[G, ?], A, B] {

      def monad: Monad[F[G, ?]] = monadTrans(rest.monad)

      def lift(fa: H[A]): F[G, A] = monadTrans.liftM(rest.lift(fa))(rest.monad)
    }

  implicit def scalazMonadTransformerCpsApply0[F[_[_], _], G[_], A, B](
      implicit monadTrans: MonadTrans[F],
      monad0: Monad[G]): ScalazBindTransformerCpsApply[G, F[G, ?], A, B] =
    new ScalazBindTransformerCpsApply[G, F[G, ?], A, B] {
      def monad = monadTrans(monad0)

      def lift(fa: G[A]): F[G, A] = monadTrans.liftM(fa)
    }

  implicit def scalazBindCpsApply[F[_], A, B](implicit bind: Bind[F]): Continuation[ScalazBind[F, A], F[B], A] =
    new Continuation[ScalazBind[F, A], F[B], A] {
      def cpsApply(instruction: ScalazBind[F, A], handler: A => F[B]): F[B] = {
        bind.bind(instruction.fa)(handler)
      }
    }

  trait ScalazBindTransformerCpsApply[F[_], G[_], A, B] extends Continuation[ScalazBind[F, A], G[B], A] {
    def monad: Monad[G]

    def lift(fa: F[A]): G[A]

    def cpsApply(instruction: ScalazBind[F, A], handler: A => G[B]): G[B] = {
      monad.bind(lift(instruction.fa))(handler)
    }
  }

}
