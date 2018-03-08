package com.thoughtworks.dsl
package domains

import _root_.cats.FlatMap
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword
import _root_.cats.MonadError
import com.thoughtworks.dsl.keywords.{Catch, Monadic, Scope}

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Exception.Catcher
import scala.util.control.{ControlThrowable, NonFatal}

/** Contains interpreters to enable [[Dsl.Keyword#unary_$bang !-notation]]
  * for [[keywords.Monadic]] and other keywords
  * in code blocks whose type support [[cats.FlatMap]] and [[cats.MonadError]].
  *
  * @example [[cats.free.Trampoline Trampoline]] is a monadic data type that performs tail call optimization.
  *          It can be built from a `@reset` code block within some [[Dsl.Keyword#unary_$bang !-notation]],
  *          similar to the [[com.thoughtworks.each.Monadic.EachOps#each each]] method in
  *          [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]].
  *
  *          {{{
  *          import cats.free.Trampoline
  *          import cats.instances.function._
  *          import com.thoughtworks.dsl.keywords.Monadic._
  *          import com.thoughtworks.dsl.domains.cats._
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
  *          Thus, the method `dslSquare` is equivalent to the following code in [[cats.syntax]]:
  *
  *          {{{
  *
  *          def catsSyntaxSquare = trampoline3.flatMap { tmp1 =>
  *            import cats.syntax.flatMap._
  *            trampoline3.flatMap { tmp2 =>
  *              Trampoline.delay {
  *                s"This string is produced by a trampoline: ${tmp1 * tmp2}"
  *              }
  *            }
  *          }
  *
  *          catsSyntaxSquare.run should be("This string is produced by a trampoline: 9")
  *          }}}
  *
  *          <hr/>
  *
  *          A `@reset` code block can contain `try` / `catch` / `finally`
  *          if the monadic data type supports [[cats.MonadError]].
  *
  *          [[cats.effect.IO]] is a monadic data type that supports [[cats.MonadError]],
  *          therefore `try` / `catch` / `finally` expressions can be used inside a `@reset` code block
  *          whose return type is [[cats.effect.IO]].
  *
  *          {{{
  *          import cats.effect.IO
  *          val io0 = IO(0)
  *
  *          def dslTryCatch: IO[String] = IO {
  *            try {
  *              s"Division result: ${!io0 / !io0}"
  *            } catch {
  *              case e: ArithmeticException =>
  *                s"Cannot divide ${!io0} by itself"
  *            }
  *          }: @reset
  *
  *          dslTryCatch.unsafeRunSync() should be("Cannot divide 0 by itself")
  *          }}}
  *
  *          The above `dslTryCatch` method is equivalent to the following code in [[cats.syntax]]:
  *
  *          {{{
  *          def catsSyntaxTryCatch: IO[String] = {
  *            import cats.syntax.applicativeError._
  *            io0.flatMap { tmp0 =>
  *              io0.flatMap { tmp1 =>
  *                 IO(s"Division result: ${tmp0 / tmp1}")
  *              }
  *            }.handleErrorWith {
  *              case e: ArithmeticException =>
  *                io0.flatMap { tmp2 =>
  *                   IO(s"Cannot divide ${tmp2} by itself")
  *                }
  *              case e =>
  *                e.raiseError[IO, String]
  *            }
  *          }
  *
  *          catsSyntaxTryCatch.unsafeRunSync() should be("Cannot divide 0 by itself")
  *          }}}
  *
  * @author 杨博 (Yang Bo)
  * @todo [[Monadic]] should be a [[scala.AnyVal]] after [[https://github.com/scala/bug/issues/10595]] is resolved.
  */
object cats {
  protected type MonadThrowable[F[_]] = MonadError[F, Throwable]

  implicit def catsScopeDsl[F[_], A, B](implicit monadError: MonadError[F, Throwable]): Dsl[Scope[F[B], A], F[B], A] =
    new Dsl[Scope[F[B], A], F[B], A] {
      def interpret(keyword: Scope[F[B], A], handler: A => F[B]): F[B] = {
        val continuation: (A => F[B]) => F[B] = keyword.continuation
        final case class BreakScope(a: A) extends ControlThrowable
        monadError.handleErrorWith(continuation { a =>
          monadError.raiseError(BreakScope(a))
        }) {
          case BreakScope(a) =>
            handler(a)
          case e: Throwable =>
            monadError.raiseError(e)
        }
      }
    }

  implicit def catsCatchDsl[F[_], A](implicit monadError: MonadThrowable[F]): Dsl[Catch[F[A]], F[A], Unit] =
    new Dsl[Catch[F[A]], F[A], Unit] {
      def interpret(keyword: Catch[F[A]], continuation: Unit => F[A]): F[A] = {
        monadError.handleErrorWith(try {
          continuation(())
        } catch {
          case e: Throwable =>
            monadError.raiseError[A](e)
        })(keyword.failureHandler)
      }
    }

  implicit def catsFlatMapDsl[F[_], A, B](implicit flatMap: FlatMap[F]): Dsl[Monadic[F, A], F[B], A] =
    new Dsl[Monadic[F, A], F[B], A] {
      def interpret(keyword: Monadic[F, A], handler: A => F[B]): F[B] = {
        flatMap.flatMap(keyword.fa)(handler)
      }
    }

}
