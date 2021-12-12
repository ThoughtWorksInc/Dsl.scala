package com.thoughtworks.dsl
package keywords
import Dsl.AsKeyword

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.keywords.Shift.{SameDomainStackSafeShiftDsl, StackSafeShiftDsl}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec

/** @author
  *   杨博 (Yang Bo)
  */
opaque type Shift[R, A] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[R !! A]

private[keywords] trait LowPriorityShift1 {

  @inline
  implicit def stackUnsafeShiftDsl[Domain, Value]: Dsl[Shift[Domain, Value], Domain, Value] =
    new Dsl[Shift[Domain, Value], Domain, Value] {
      def cpsApply(shift: Shift[Domain, Value], handler: Value => Domain) =
        Shift.apply.flip(shift)(handler)
    }

}

private[keywords] trait LowPriorityShift0 extends LowPriorityShift1 { this: Shift.type =>

  given [LeftDomain, RightDomain, Value](using
      restDsl: SameDomainStackSafeShiftDsl[LeftDomain, RightDomain]
  ): SameDomainStackSafeShiftDsl[LeftDomain !! RightDomain, Value] = {
    val restDsl0 = apply.liftContra[[X] =>> Dsl[X, LeftDomain, RightDomain]](restDsl);
    { (keyword, handler) =>
      keyword { value =>
        restDsl0.cpsApply(handler(value), _)
      }
    }
  }

}

object Shift extends LowPriorityShift0 {
  given [Domain, Value]: AsKeyword.IsKeyword[Shift[Domain, Value], Value] with {}

  trait StackSafeShiftDsl[Domain, NewDomain, Value] extends Dsl[Shift[Domain, Value], NewDomain, Value]

  private[keywords] type SameDomainStackSafeShiftDsl[Domain, Value] = StackSafeShiftDsl[Domain, Domain, Value]

  given implicitShift[Domain, Value]: AsKeyword[Domain !! Value, Shift[Domain, Value], Value] = Shift(_)

  private def shiftTailRec[R, Value](continuation: TailRec[R] !! Value, handler: Value => TailRec[R]) = {
    continuation { a =>
      val handler1 = handler
      TailCalls.tailcall(handler1(a))
    }
  }

  @inline
  implicit def tailRecShiftDsl[R, Value]: SameDomainStackSafeShiftDsl[TailRec[R], Value] =
    new SameDomainStackSafeShiftDsl[TailRec[R], Value] {
      def cpsApply(keyword: Shift[TailRec[R], Value], handler: Value => TailRec[R]): TailRec[R] = {
        shiftTailRec(keyword, handler)
      }
    }

  private abstract class TrampolineContinuation[LeftDomain] extends (LeftDomain !! Throwable) {
    protected def step(): LeftDomain !! Throwable

    @tailrec
    private final def last(): LeftDomain !! Throwable = {
      step() match {
        case trampoline: TrampolineContinuation[LeftDomain] =>
          trampoline.last()
        case notTrampoline =>
          notTrampoline
      }
    }

    final def apply(handler: Throwable => LeftDomain): LeftDomain = {
      val protectedContinuation: LeftDomain !! Throwable =
        try {
          last()
        } catch {
          case NonFatal(e) =>
            return handler(e)
        }
      protectedContinuation(handler)
    }
  }

  private def suspend[LeftDomain, Value](
      continuation: LeftDomain !! Throwable !! Value,
      handler: Value => LeftDomain !! Throwable
  ): TrampolineContinuation[LeftDomain] =
    new TrampolineContinuation[LeftDomain] {
      protected def step() = continuation(handler)
    }

  @inline
  implicit def stackSafeThrowableShiftDsl[LeftDomain, Value]
      : SameDomainStackSafeShiftDsl[LeftDomain !! Throwable, Value] =
    new SameDomainStackSafeShiftDsl[LeftDomain !! Throwable, Value] {

      def cpsApply(
          keyword: Shift[LeftDomain !! Throwable, Value],
          handler: Value => LeftDomain !! Throwable
      ): !![LeftDomain, Throwable] =
        suspend(keyword, handler)
    }

  private def flatMapTrampoline[LeftDomain, RightDomain, Value](
      handler: Value => LeftDomain !! Throwable !! RightDomain,
      value: Value,
      continue: RightDomain => LeftDomain !! Throwable
  ): TrampolineContinuation[LeftDomain] =
    new TrampolineContinuation[LeftDomain] {
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
  implicit def taskStackSafeShiftDsl[LeftDomain, RightDomain, Value]
      : StackSafeShiftDsl[LeftDomain !! Throwable, LeftDomain !! Throwable !! RightDomain, Value] =
    new StackSafeShiftDsl[LeftDomain !! Throwable, LeftDomain !! Throwable !! RightDomain, Value] {
      def cpsApply(
          keyword: Shift[!![LeftDomain, Throwable], Value],
          handler: Value => !![!![LeftDomain, Throwable], RightDomain]
      ): !![!![LeftDomain, Throwable], RightDomain] =
        taskFlatMap(keyword, handler)
    }

  def apply[R, A]: (R !! A) =:= Shift[R, A] = Dsl.Keyword.Opaque.Of.apply

}
