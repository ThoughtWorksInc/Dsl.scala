package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import scalaz.Leibniz.===

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Bind, Monad, MonadError, MonadTrans, Unapply}

import scala.util.control.Exception.Catcher
import scala.util.control.{ControlThrowable, NonFatal}

/** This [[ScalazBind]] enables [[Dsl.Instruction#unary_$bang !-notation]] for types
  * that support [[scalaz.Bind]], [[scalaz.MonadError]] and [[scalaz.MonadTrans]].
  *
  * @example [[scalaz.Free.Trampoline Trampoline]] is a monadic data type that performs tail call optimization.
  *          It can be built from a `@reset` code block within some [[Dsl.Instruction#unary_$bang !-notation]],
  *          similar to the [[com.thoughtworks.each.Monadic.EachOps#each each]] method in
  *          [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]].
  *
  *          {{{
  *          import scalaz.Trampoline
  *          import scalaz.Free.Trampoline
  *          import com.thoughtworks.dsl.instructions.ScalazBind._
  *          import com.thoughtworks.dsl.Dsl.reset
  *
  *          val trampoline3 = Trampoline.done(3)
  *
  *          def dslSquare = Trampoline.delay {
  *            s"This string is produced by a trampoline: ${!trampoline3 * !trampoline3}"
  *          }: @reset
  *
  *          dslSquare.run should be("This string is produced by a trampoline: 9")
  *          }}}
  *
  *          `!trampoline3` is a shortcut of `!ScalazBind(trampoline3)`,
  *          which will be converted to `flatMap` calls by our DSL interpreter.
  *          Thus, the method `dslSquare` is equivalent to the following code in [[scalaz.syntax]]:
  *
  *          {{{
  *
  *          def scalazSyntaxSquare = trampoline3.flatMap { tmp1 =>
  *            import scalaz.syntax.bind._
  *            trampoline3.flatMap { tmp2 =>
  *              Trampoline.delay {
  *                s"This string is produced by a trampoline: ${tmp1 * tmp2}"
  *              }
  *            }
  *          }
  *
  *          scalazSyntaxSquare.run should be("This string is produced by a trampoline: 9")
  *          }}}
  *
  *          <hr/>
  *
  *          A `@reset` code block can contain `try` / `catch` / `finally`
  *          if the monadic data type supports [[scalaz.MonadError]].
  *
  *          [[https://github.com/ThoughtWorksInc/tryt.scala tryt.scala]] is a monad transformer that provides
  *          [[scalaz.MonadError]],
  *          therefore `try` / `catch` / `finally` expressions can be used inside a `@reset` code block
  *          whose return type is `TryT[Trampoline, ?]`.
  *
  *          {{{
  *          import com.thoughtworks.tryt.invariant.TryT, TryT._
  *          import scala.util.{Try, Success}
  *          type TryTTransfomredTrampoline[A] = TryT[Trampoline, A]
  *
  *          val trampolineSuccess0: TryTTransfomredTrampoline[Int] = TryT(Trampoline.done(Try(0)))
  *
  *          def dslTryCatch: TryTTransfomredTrampoline[String] = TryT(Trampoline.delay(Try {
  *            try {
  *              s"Division result: ${!trampoline3 / !trampolineSuccess0}"
  *            } catch {
  *              case e: ArithmeticException =>
  *                s"Cannot divide ${!trampoline3} by ${!trampolineSuccess0}"
  *            }
  *          })): @reset
  *
  *          inside(dslTryCatch) {
  *            case TryT(trampoline) =>
  *              trampoline.run should be(Success("Cannot divide 3 by 0"))
  *          }
  *          }}}
  *
  *          Note that [[Dsl.Instruction#unary_$bang !-notation]] can be used on
  *          both `trampoline3` and `trampolineSuccess0` even when they are different types,
  *          i.e. `trampoline3` is a vanilla [[scalaz.Free.Trampoline Trampoline]],
  *          while `trampolineSuccess0` is a [[com.thoughtworks.tryt.invariant.TryT TryT]]-transfomred
  *          [[scalaz.Free.Trampoline Trampoline]].
  *          It is possible because the interpreters of this [[ScalazBind]] instruction invoke
  *          [[scalaz.MonadTrans.liftM]] automatically.
  *
  *          The above `dslTryCatch` method is equivalent to the following code in [[scalaz.syntax]]:
  *
  *          {{{
  *          def scalazSyntaxTryCatch: TryTTransfomredTrampoline[String] = {
  *            import scalaz.syntax.monadError._
  *            trampoline3.liftM[TryT].flatMap { tmp0 =>
  *              trampolineSuccess0.flatMap { tmp1 =>
  *                 TryT(Trampoline.delay(Try(s"Division result: ${tmp0 / tmp1}")))
  *              }
  *            }.handleError {
  *              case e: ArithmeticException =>
  *                trampoline3.liftM[TryT].flatMap { tmp2 =>
  *                  trampolineSuccess0.flatMap { tmp3 =>
  *                     TryT(Trampoline.delay(Try(s"Cannot divide ${tmp2} by ${tmp3}")))
  *                  }
  *                }
  *              case e =>
  *                e.raiseError[TryTTransfomredTrampoline, String]
  *            }
  *          }
  *
  *          inside(scalazSyntaxTryCatch) {
  *            case TryT(trampoline) =>
  *              trampoline.run should be(Success("Cannot divide 3 by 0"))
  *          }
  *          }}}
  *
  * @author 杨博 (Yang Bo)
  * @todo [[ScalazBind]] should be a [[scala.AnyVal]] after [[https://github.com/scala/bug/issues/10595]] is resolved.
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
