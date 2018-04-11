package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Shift.StackSafeShiftDsl

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec

/**
  * @author 杨博 (Yang Bo)
  */
final case class Shift[Domain, Value](continuation: Domain !! Value)
    extends AnyVal
    with Keyword[Shift[Domain, Value], Value]

private[keywords] trait LowPriorityShift1 {

  implicit def stackUnsafeShiftDsl[Domain, Value]: Dsl[Shift[Domain, Value], Domain, Value] =
    new Dsl[Shift[Domain, Value], Domain, Value] {
      def interpret(shift: Shift[Domain, Value], mapper: Value => Domain): Domain =
        shift.continuation(mapper)
    }

}

private[keywords] trait LowPriorityShift0 extends LowPriorityShift1 {

  @inline
  implicit def stackSafeShiftDsl[Domain, Value](
      implicit stackSafeShiftDsl: StackSafeShiftDsl[Domain, Value]): Dsl[Shift[Domain, Value], Domain, Value] = {
    stackSafeShiftDsl
  }

}

object Shift extends LowPriorityShift0 {
  trait StackSafeShiftDsl[Domain, Value] extends Dsl[Shift[Domain, Value], Domain, Value]

  implicit def implicitShift[Domain, Value](fa: Domain !! Value): Shift[Domain, Value] =
    Shift[Domain, Value](fa)

  implicit def tailRecShiftDsl[R, Value]: StackSafeShiftDsl[TailRec[R], Value] =
    new StackSafeShiftDsl[TailRec[R], Value] {
      def interpret(keyword: Shift[TailRec[R], Value], handler: Value => TailRec[R]): TailRec[R] = {
        keyword.continuation { a =>
          TailCalls.tailcall(handler(a))
        }
      }
    }

  private[Shift] final case class TrampolineContinuation[LeftDomain, Value](
      continuation: LeftDomain !! Throwable !! Value,
      handler: Value => LeftDomain !! Throwable)
      extends (LeftDomain !! Throwable) {

    @tailrec
    @inline
    private def last(): LeftDomain !! Throwable = {
      continuation(handler) match {
        case trampoline: TrampolineContinuation[LeftDomain, Value] =>
          trampoline.last()
        case notTrampoline =>
          notTrampoline
      }
    }

    def apply(raiiHandler: Throwable => LeftDomain): LeftDomain = {
      val protectedContinuation: LeftDomain !! Throwable = try {
        last()
      } catch {
        case NonFatal(e) =>
          return raiiHandler(e)
      }
      protectedContinuation(raiiHandler)
    }
  }

  @inline
  implicit def stackSafeThrowableShiftDsl[LeftDomain, Value]: StackSafeShiftDsl[LeftDomain !! Throwable, Value] =
    new StackSafeShiftDsl[LeftDomain !! Throwable, Value] {
      @inline
      def interpret(keyword: Shift[LeftDomain !! Throwable, Value],
                    handler: Value => LeftDomain !! Throwable): LeftDomain !! Throwable = {
        TrampolineContinuation(keyword.continuation, handler)
      }
    }
//  TODO: StackSafeShift for `LeftDomain !! Throwable`
}
