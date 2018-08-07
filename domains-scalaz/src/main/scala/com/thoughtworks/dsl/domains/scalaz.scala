package com.thoughtworks.dsl.domains

import com.thoughtworks.Extractor._
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.language.higherKinds
import scala.language.implicitConversions
import _root_.scalaz.{Applicative, Bind, Monad, MonadError, MonadTrans, Unapply}
import com.thoughtworks.dsl.keywords.Catch.CatchDsl
import com.thoughtworks.dsl.keywords.{Catch, Monadic, Return}

import scala.util.control.Exception.Catcher
import scala.util.control.{ControlThrowable, NonFatal}

/** Contains interpreters to enable [[Dsl.Keyword#unary_$bang !-notation]]
  * for [[keywords.Monadic]] and other keywords
  * in code blocks whose type support [[scalaz.Bind]], [[scalaz.MonadError]] and [[scalaz.MonadTrans]].
  *
  * @example [[scalaz.Free.Trampoline]] is a monadic data type that performs tail call optimization.
  *          It can be built from a `@[[Dsl.reset reset]]` code block within some [[Dsl.Keyword#unary_$bang !-notation]],
  *          similar to the [[com.thoughtworks.each.Monadic.EachOps#each each]] method in
  *          [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]].
  *
  *          {{{
  *          import _root_.scalaz.Trampoline
  *          import _root_.scalaz.Free.Trampoline
  *          import com.thoughtworks.dsl.keywords.Monadic._
  *          import com.thoughtworks.dsl.domains.scalaz._
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
  *          `!trampoline3` is a shortcut of `!Monadic(trampoline3)`,
  *          which will be converted to `flatMap` calls by our DSL interpreter.
  *          Thus, the method `dslSquare` is equivalent to the following code in [[scalaz.syntax]]:
  *
  *          {{{
  *
  *          def scalazSyntaxSquare = trampoline3.flatMap { tmp1 =>
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
  *          A `@[[Dsl.reset reset]]` code block can contain `try` / `catch` / `finally`
  *          if the monadic data type supports [[scalaz.MonadError]].
  *
  *          [[https://github.com/ThoughtWorksInc/tryt.scala tryt.scala]] is a monad transformer that provides
  *          [[scalaz.MonadError]],
  *          therefore `try` / `catch` / `finally` expressions can be used inside a `@[[Dsl.reset reset]]` code block
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
  *          Note that [[Dsl.Keyword#unary_$bang !-notation]] can be used on
  *          both `trampoline3` and `trampolineSuccess0` even when they are different types,
  *          i.e. `trampoline3` is a vanilla [[scalaz.Free.Trampoline Trampoline]],
  *          while `trampolineSuccess0` is a [[com.thoughtworks.tryt.invariant.TryT TryT]]-transfomred
  *          [[scalaz.Free.Trampoline Trampoline]].
  *          It is possible because the interpreters of this [[Monadic]] keyword invoke
  *          [[scalaz.MonadTrans.liftM]] automatically.
  *
  *          The above `dslTryCatch` method is equivalent to the following code in [[scalaz.syntax]]:
  *
  *          {{{
  *          def scalazSyntaxTryCatch: TryTTransfomredTrampoline[String] = {
  *            import _root_.scalaz.syntax.monadError._
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
  */
object scalaz {

  protected type MonadThrowable[F[_]] = MonadError[F, Throwable]

  implicit def scalazCatchDsl[F[_], A, B](implicit monadError: MonadThrowable[F]): CatchDsl[F[A], F[B], A] =
    new CatchDsl[F[A], F[B], A] {
      def tryCatch(block: F[A] !! A, catcher: Catcher[F[A] !! A], handler: A => F[B]): F[B] = {
        import _root_.scalaz.syntax.all._
        val fa = monadError.bind(monadError.pure(block))(_(monadError.pure(_)))
        val protectedFa = monadError.handleError(fa) {
          case catcher.extract(recovered) =>
            recovered(monadError.pure(_))
          case e =>
            monadError.raiseError[A](e)
        }
        monadError.bind(protectedFa)(handler)
      }
    }

  implicit def scalazReturnDsl[F[_], A, B, R](implicit applicative: Applicative[F],
                                              restReturnDsl: Dsl[Return[A, B], B, B]) =
    new Dsl[Return[A, R], F[B], Nothing] {
      def cpsApply(keyword: Return[A, R], handler: Nothing => F[B]): F[B] = {
        applicative.pure(restReturnDsl.cpsApply(Return(keyword.returnValue), identity))
      }
    }

  implicit def scalazMonadTransformerDsl1[F[_[_], _], H[_], G[_], A, B](
      implicit monadTrans: MonadTrans[F],
      rest: ScalazTransformerDsl[H, G, A, B]): ScalazTransformerDsl[H, F[G, ?], A, B] =
    new ScalazTransformerDsl[H, F[G, ?], A, B] {

      def monad: Monad[F[G, ?]] = monadTrans(rest.monad)

      def lift(fa: H[A]): F[G, A] = monadTrans.liftM(rest.lift(fa))(rest.monad)

    }

  implicit def scalazMonadTransformerDsl0[F[_[_], _], G[_], A, B](
      implicit monadTrans: MonadTrans[F],
      monad0: Monad[G]): ScalazTransformerDsl[G, F[G, ?], A, B] =
    new ScalazTransformerDsl[G, F[G, ?], A, B] {
      def monad = monadTrans(monad0)

      def lift(fa: G[A]): F[G, A] = monadTrans.liftM(fa)

    }

  implicit def scalazMonadicDsl[F[_], A, B](implicit bind: Bind[F]): Dsl[Monadic[F, A], F[B], A] =
    new Dsl[Monadic[F, A], F[B], A] {
      def cpsApply(keyword: Monadic[F, A], handler: A => F[B]): F[B] = {
        bind.bind(keyword.fa)(handler)
      }
    }

  abstract class ScalazTransformerDsl[F[_], G[_], A, B] extends Dsl[Monadic[F, A], G[B], A] {
    def monad: Monad[G]

    def lift(fa: F[A]): G[A]

    final def cpsApply(keyword: Monadic[F, A], handler: A => G[B]): G[B] = {
      monad.bind(lift(keyword.fa))(handler)
    }

  }

}
