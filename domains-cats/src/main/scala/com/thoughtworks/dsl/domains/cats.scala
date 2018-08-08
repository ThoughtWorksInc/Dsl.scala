package com.thoughtworks.dsl
package domains

import _root_.cats.{Applicative, FlatMap}
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import _root_.cats.MonadError
import com.thoughtworks.Extractor._
import com.thoughtworks.dsl.keywords.Catch.CatchDsl
import com.thoughtworks.dsl.keywords.{Catch, Monadic, Return}

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Exception.Catcher
import scala.util.control.{ControlThrowable, NonFatal}

/** Contains interpreters to enable [[Dsl.Keyword#unary_$bang !-notation]]
  * for [[keywords.Monadic]] and other keywords
  * in code blocks whose type support [[cats.FlatMap]] and [[cats.MonadError]].
  *
  * @example [[cats.free.Trampoline Trampoline]] is a monadic data type that performs tail call optimization.
  *          It can be built from a `@[[Dsl.reset reset]]` code block within some [[Dsl.Keyword#unary_$bang !-notation]],
  *          similar to the [[com.thoughtworks.each.Monadic.EachOps#each each]] method in
  *          [[https://github.com/ThoughtWorksInc/each ThoughtWorks Each]].
  *
  *          {{{
  *          import _root_.cats.free.Trampoline
  *          import _root_.cats.instances.function._
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
  *            trampoline3.flatMap { tmp2 =>
  *              Trampoline.delay {
  *                s"This string is produced by a trampoline: ${tmp1 * tmp2}"
  *              }
  *            }
  *          }
  *
  *          catsSyntaxSquare.run should be("This string is produced by a trampoline: 9")
  *          }}}
  * @example A `@[[Dsl.reset reset]]` code block can contain `try` / `catch` / `finally`
  *          if the monadic data type supports [[cats.MonadError]].
  *
  *          For example, [[cats.effect.IO]] is a monadic data type that supports [[cats.MonadError]],
  *          therefore `try` / `catch` / `finally` expressions can be used inside a `@[[Dsl.reset reset]]` code block
  *          whose return type is [[cats.effect.IO]].
  *
  *          {{{
  *          import com.thoughtworks.dsl.keywords.Monadic._
  *          import com.thoughtworks.dsl.domains.cats._
  *          import _root_.cats.effect.IO
  *          import com.thoughtworks.dsl.Dsl.reset
  *
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
  *            import _root_.cats.syntax.applicativeError._
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
  * @author 杨博 (Yang Bo)
  */
object cats {
  protected type MonadThrowable[F[_]] = MonadError[F, Throwable]

  implicit def catsReturnDsl[F[_], A, B](implicit applicative: Applicative[F],
                                         restReturnDsl: Dsl[Return[A], B, Nothing]) =
    new Dsl[Return[A], F[B], Nothing] {
      def cpsApply(keyword: Return[A], handler: Nothing => F[B]): F[B] = {
        applicative.pure(restReturnDsl.cpsApply(keyword, identity))
      }
    }

  implicit def catsCatchDsl[F[_], A, B](implicit monadError: MonadThrowable[F]): CatchDsl[F[A], F[B], A] =
    new CatchDsl[F[A], F[B], A] {
      def tryCatch(block: F[A] !! A, catcher: Catcher[F[A] !! A], handler: A => F[B]): F[B] = {
        val fa = monadError.flatMap(monadError.pure(block))(_(monadError.pure(_)))
        val protectedFa = monadError.handleErrorWith(fa) {
          case catcher.extract(recovered) =>
            recovered(monadError.pure(_))
          case e =>
            monadError.raiseError[A](e)
        }
        monadError.flatMap(protectedFa)(handler)
      }
    }

  implicit def catsMonadicDsl[F[_], A, B](implicit flatMap: FlatMap[F]): Dsl[Monadic[F, A], F[B], A] =
    new Dsl[Monadic[F, A], F[B], A] {
      def cpsApply(keyword: Monadic[F, A], handler: A => F[B]): F[B] = {
        flatMap.flatMap(keyword.fa)(handler)
      }
    }

}
