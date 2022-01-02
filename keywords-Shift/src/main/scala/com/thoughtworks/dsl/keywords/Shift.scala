package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.keywords.Shift.{
  SameDomainStackSafeShiftDsl,
  StackSafeShiftDsl
}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec

/** A keyword that extracts the value from a [[domains.Continuation]].
  * @note
  *   This [[Shift]] keyword includes special treatment for exception handling
  *   and stack safety. Always use `Shift(cont).cpsApply { x => ... }` instead
  *   of `cont { x => ... }` to register a handler for the continuation,
  *   otherwise exception might be uncaught or stack might overflow.
  * @example
  *   Given a continuation whose type is `Unit !! Throwable !! Int`, it is
  *   considered as having an exception handler. When an exception is thrown,
  * {{{
  *   import scala.util.control.TailCalls.TailRec
  *   type !![R, +A] = (A => R) => R
  *   val cont: Unit !! Throwable !! Int = _ {
  *     if (System.nanoTime() > 0) {
  *       throw new Exception("my-exception")
  *     } else {
  *       42
  *     }
  *   }
  * }}}
  *
  * Then `cpsApply` should catch the exception:
  * {{{
  *   Shift(cont).cpsApply[Unit !! Throwable] { i: Int => (failureHandler: Throwable => Unit) =>
  *     fail("unreachable code")
  *   } { e: Throwable =>
  *     e.getMessage should be("my-exception")
  *   }
  * }}}
  *
  * However, `cont.apply` does not catch the exception:
  * {{{
  *   an[Exception] should be thrownBy {
  *     cont.apply { i => failureHandler =>
  *       fail("unreachable code")
  *     } { e =>
  *       e.getMessage should be("my-exception")
  *     }
  *   }
  * }}}
  * @author
  *   杨博 (Yang Bo)
  */
opaque type Shift[R, A] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[R !! A]

private[keywords] trait LowPriorityShift1 {

  @inline
  implicit def stackUnsafeShiftDsl[Domain, Value]
      : Dsl.Original[Shift[Domain, Value], Domain, Value] =
    Dsl.Original[Shift[Domain, Value], Domain, Value] {
      (shift: Shift[Domain, Value], handler: Value => Domain) =>
        Shift.apply.flip(shift)(handler)
    }

}

private[keywords] trait LowPriorityShift0 extends LowPriorityShift1 {
  this: Shift.type =>

  given [LeftDomain, RightDomain, Value](using
      restDsl: SameDomainStackSafeShiftDsl[LeftDomain, RightDomain]
  ): SameDomainStackSafeShiftDsl[LeftDomain !! RightDomain, Value] =
    SameDomainStackSafeShiftDsl[LeftDomain !! RightDomain, Value] {
      val restDsl0 =
        apply
          .liftContra[[X] =>> Dsl.Original[X, LeftDomain, RightDomain]](
            restDsl
          );
      { (keyword, handler) =>
        keyword { value =>
          restDsl0(handler(value), _)
        }
      }
    }

}

object Shift extends LowPriorityShift0 {
  given [Domain, Value]: IsKeyword[Shift[Domain, Value], Value] with {}

  opaque type StackSafeShiftDsl[Domain, NewDomain, Value] <: Dsl.Original[Shift[
    Domain,
    Value
  ], NewDomain, Value] = Dsl.Original[Shift[Domain, Value], NewDomain, Value]
  object StackSafeShiftDsl:
    def apply[Domain, NewDomain, Value]: (
        (
            Shift[Domain, Value],
            Value => NewDomain
        ) => NewDomain
    ) =:= StackSafeShiftDsl[Domain, NewDomain, Value] =
      Dsl.Original.apply[Shift[Domain, Value], NewDomain, Value]

  private[keywords] type SameDomainStackSafeShiftDsl[Domain, Value] =
    StackSafeShiftDsl[Domain, Domain, Value]
  object SameDomainStackSafeShiftDsl:
    def apply[Domain, Value] = StackSafeShiftDsl[Domain, Domain, Value]

  extension [C, R, A](inline fa: C)(using
      inline notKeyword: util.NotGiven[
        C <:< Dsl.Keyword
      ],
      inline asContinuation: C <:< (R !! A)
  )
    transparent inline def unary_! : A =
      Dsl.shift[Shift[R, A], A](Shift[R, A](asContinuation(fa))): A

  private def shiftTailRec[R, Value](
      continuation: TailRec[R] !! Value,
      handler: Value => TailRec[R]
  ) = {
    continuation { a =>
      val handler1 = handler
      TailCalls.tailcall(handler1(a))
    }
  }

  @inline
  given tailRecShiftDsl[R, Value]
      : SameDomainStackSafeShiftDsl[TailRec[R], Value] =
    SameDomainStackSafeShiftDsl {
      (
          keyword: Shift[TailRec[R], Value],
          handler: Value => TailRec[R]
      ) =>
        shiftTailRec(keyword, handler)

    }

  private def suspend[LeftDomain, Value](
      continuation: LeftDomain !! Throwable !! Value,
      handler: Value => LeftDomain !! Throwable
  ): Dsl.TrampolineContinuation[LeftDomain] =
    new Dsl.TrampolineContinuation[LeftDomain] {
      protected def step() = continuation(handler)
    }

  @inline
  given [LeftDomain, Value]
      : SameDomainStackSafeShiftDsl[LeftDomain !! Throwable, Value] =
    SameDomainStackSafeShiftDsl[LeftDomain !! Throwable, Value] {

      (
          keyword: Shift[LeftDomain !! Throwable, Value],
          handler: Value => LeftDomain !! Throwable
      ) =>
        suspend(keyword, handler)
    }

  private def flatMapTrampoline[LeftDomain, RightDomain, Value](
      handler: Value => LeftDomain !! Throwable !! RightDomain,
      value: Value,
      continue: RightDomain => LeftDomain !! Throwable
  ): Dsl.TrampolineContinuation[LeftDomain] =
    new Dsl.TrampolineContinuation[LeftDomain] {
      protected def step() = {
        handler(value)(continue)
      }
    }

  private def taskFlatMap[LeftDomain, RightDomain, Value](
      task: LeftDomain !! Throwable !! Value,
      handler0: Value => LeftDomain !! Throwable !! RightDomain
  ): LeftDomain !! Throwable !! RightDomain = { continue0 =>
    val handler1 = handler0
    task { value =>
      val handler = handler1
      val continue = continue0
      flatMapTrampoline(handler, value, continue)
    }
  }

  @inline
  given [LeftDomain, RightDomain, Value]: StackSafeShiftDsl[
    LeftDomain !! Throwable,
    LeftDomain !! Throwable !! RightDomain,
    Value
  ] =
    StackSafeShiftDsl[
      LeftDomain !! Throwable,
      LeftDomain !! Throwable !! RightDomain,
      Value
    ] {
      (
          keyword: Shift[!![LeftDomain, Throwable], Value],
          handler: Value => !![!![LeftDomain, Throwable], RightDomain]
      ) =>
        taskFlatMap(keyword, handler)
    }

  def apply[R, A]: (R !! A) =:= Shift[R, A] = Dsl.Keyword.Opaque.Of.apply

}
