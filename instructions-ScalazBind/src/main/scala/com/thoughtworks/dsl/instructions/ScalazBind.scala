package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Bind, Monad, MonadError, MonadTrans, Unapply}

import scala.util.control.Exception.Catcher
import scala.util.control.{ControlThrowable, NonFatal}

/**
  * @author 杨博 (Yang Bo)
  * @note [[ScalazBind]] is not an [[scala.AnyVal]] due to [[https://github.com/scala/bug/issues/10595]].
  */
final case class ScalazBind[F[_], A](fa: F[A]) extends Instruction[ScalazBind[F, A], A]

private[instructions] trait LowPriorityScalazBind0 { this: ScalazBind.type =>

  implicit def scalazCatchDslUnapply[FA](implicit unapply: Unapply[MonadThrowable, FA]): Dsl[Catch[FA], FA, Unit] = {
    unapply.leibniz.flip.subst[λ[FA => Dsl[Catch[FA], FA, Unit]]](scalazCatchDsl[unapply.M, unapply.A](unapply.TC))
  }

}

object ScalazBind extends LowPriorityScalazBind0 {

  protected type MonadThrowable[F[_]] = MonadError[F, Throwable]

  implicit def scalazScopeDsl[F[_], A, B](implicit monadError: MonadThrowable[F]): Dsl[Scope[F[B], A], F[B], A] =
    new Dsl[Scope[F[B], A], F[B], A] {
      def interpret(instruction: Scope[F[B], A], handler: A => F[B]): F[B] = {
        val continuation: (A => F[B]) => F[B] = instruction.continuation
        final case class BreakScope(a: A) extends ControlThrowable
        monadError.handleError(continuation { a =>
          monadError.raiseError(BreakScope(a))
        }) {
          case BreakScope(a) =>
            handler(a)
          case e: Throwable =>
            monadError.raiseError(e)
        }
      }
    }

  implicit def scalazCatchDsl[F[_], A](implicit monadError: MonadThrowable[F]): Dsl[Catch[F[A]], F[A], Unit] =
    new Dsl[Catch[F[A]], F[A], Unit] {
      def interpret(instruction: Catch[F[A]], continuation: Unit => F[A]): F[A] = {
        monadError.handleError(try {
          continuation(())
        } catch {
          case e: Throwable =>
            monadError.raiseError[A](e)
        })(instruction.failureHandler)
      }
    }

  implicit final class ScalazMonadErrorCatchOps[F[_], A](catcher: Catcher[F[A]])(
      implicit monadError: MonadThrowable[F]) {
    def cpsCatch(continuation: (F[A] => F[A]) => F[A]): F[A] = {
      try {
        continuation { fa: F[A] =>
          monadError.handleError(fa) { e =>
            catcher.applyOrElse(e, monadError.raiseError)
          }
        }
      } catch {
        case e: Throwable =>
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

    }

  implicit def scalazMonadTransformerDsl0[F[_[_], _], G[_], A, B](
      implicit monadTrans: MonadTrans[F],
      monad0: Monad[G]): ScalazBindTransformerDsl[G, F[G, ?], A, B] =
    new ScalazBindTransformerDsl[G, F[G, ?], A, B] {
      def monad = monadTrans(monad0)

      def lift(fa: G[A]): F[G, A] = monadTrans.liftM(fa)

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

    final def interpret(instruction: ScalazBind[F, A], handler: A => G[B]): G[B] = {
      monad.bind(lift(instruction.fa))(handler)
    }

  }

}
