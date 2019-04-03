package com.thoughtworks.dsl.keywords

import com.thoughtworks.Extractor._
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Continuation, Keyword}
import com.thoughtworks.dsl.keywords.Catch.CatchDsl

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain, Value](block: Domain !! Value, catcher: Catcher[Domain !! Value])
    extends Keyword[Catch[Domain, Value], Value]
private[keywords] trait LowPriorityCatch1 {

  implicit def liftFunction1CatchDsl[InnerDomain, OuterDomain, State, Value](
      implicit leftCatchDsl: CatchDsl[InnerDomain, OuterDomain, Value])
    : CatchDsl[State => InnerDomain, State => OuterDomain, Value] = {
    new CatchDsl[State => InnerDomain, State => OuterDomain, Value] {
      def tryCatch(block: (State => InnerDomain) !! Value,
                   catcher: Catcher[(State => InnerDomain) !! Value],
                   handler: Value => State => OuterDomain): State => OuterDomain = { state =>
        leftCatchDsl.tryCatch(
          block = { (continue: Value => InnerDomain) =>
            block { value: Value => _ =>
              continue(value)
            }(state)
          },
          catcher = {
            case catcher.extract(recoveredValueContinuation) =>
              continue =>
                recoveredValueContinuation { value: Value => _ =>
                  continue(value)
                }(state)
          },
          handler = handler(_)(state)
        )
      }
    }
  }

}

private[keywords] trait LowPriorityCatch0 extends LowPriorityCatch1 { this: Catch.type =>

  implicit def liftContinuationCatchDsl[LeftDomain, RightDomain, Value](
      implicit leftCatchDsl: CatchDsl[LeftDomain, LeftDomain, Value])
    : CatchDsl[LeftDomain !! Value, LeftDomain !! RightDomain, Value] = {
    new CatchDsl[LeftDomain !! Value, LeftDomain !! RightDomain, Value] {
      def tryCatch(block: LeftDomain !! Value !! Value,
                   catcher: Catcher[LeftDomain !! Value !! Value],
                   handler: Value => LeftDomain !! RightDomain): LeftDomain !! RightDomain = { outerHandler =>
        leftCatchDsl.tryCatch(
          block = block(Continuation.now),
          catcher = {
            case catcher.extract(recoveredValueContinuation) =>
              recoveredValueContinuation(Continuation.now)
          },
          handler = { value: Value =>
            handler(value)(outerHandler)
          }
        )
      }
    }
  }

}

object Catch extends LowPriorityCatch0 {

  @implicitNotFound(
    "Statements `try` / `catch` / `finally` are not supported in the domain ${OuterDomain} when the return value is ${Value} and the inner domain is ${InnerDomain}.")
  trait CatchDsl[InnerDomain, OuterDomain, Value] extends Dsl[Catch[InnerDomain, Value], OuterDomain, Value] {

    def tryCatch(block: InnerDomain !! Value,
                 catcher: Catcher[InnerDomain !! Value],
                 handler: Value => OuterDomain): OuterDomain

    @inline final def cpsApply(keyword: Catch[InnerDomain, Value], handler: Value => OuterDomain): OuterDomain = {
      tryCatch(keyword.block, keyword.catcher, handler)
    }
  }

  @inline
  def tryCatch[InnerDomain, OuterDomain, Value](finalizer: Value => OuterDomain)(
      implicit catchDsl: CatchDsl[InnerDomain, OuterDomain, Value]) = {
    (block: InnerDomain !! Value, catcher: Catcher[InnerDomain !! Value]) =>
      catchDsl.tryCatch(block, catcher, finalizer)
  }

  implicit def futureCatchDsl[InnerValue, OuterValue](
      implicit executionContext: ExecutionContext): CatchDsl[Future[InnerValue], Future[OuterValue], InnerValue] =
    new CatchDsl[Future[InnerValue], Future[OuterValue], InnerValue] {
      def tryCatch(block: Future[InnerValue] !! InnerValue,
                   catcher: Catcher[Future[InnerValue] !! InnerValue],
                   handler: InnerValue => Future[OuterValue]): Future[OuterValue] = {
        val fa = Future(block).flatMap(_(Future.successful))
        val protectedFa = fa.recoverWith {
          case catcher.extract(recovered) =>
            recovered(Future.successful)
        }
        protectedFa.flatMap(handler)
      }
    }

  implicit def throwableCatchDsl[LeftDomain, Value](
      implicit shiftDsl: Dsl[Shift[LeftDomain, Throwable], LeftDomain, Throwable])
    : CatchDsl[LeftDomain !! Throwable, LeftDomain !! Throwable, Value] =
    new CatchDsl[LeftDomain !! Throwable, LeftDomain !! Throwable, Value] {
      @inline
      def tryCatch(block: LeftDomain !! Throwable !! Value,
                   catcher: Catcher[LeftDomain !! Throwable !! Value],
                   handler: Value => LeftDomain !! Throwable): LeftDomain !! Throwable = {
        new (LeftDomain !! Throwable) {
          def apply(outerFailureHandler: Throwable => LeftDomain): LeftDomain = {

            def recover(e: Throwable): LeftDomain = {
              e match {
                case catcher.extract(recovered) =>
                  val outerContinuation = try {
                    recovered(handler)
                  } catch {
                    case NonFatal(e) =>
                      return outerFailureHandler(e)
                  }
                  outerContinuation(outerFailureHandler)
                case e =>
                  outerFailureHandler(e)
              }
            }

            val protectedContinuation = try {
              block { value =>
                new (LeftDomain !! Throwable) {
                  def apply(ignored: Throwable => LeftDomain): LeftDomain = {
                    val rest = try {
                      handler(value)
                    } catch {
                      case NonFatal(e) =>
                        return outerFailureHandler(e)
                    }
                    rest(outerFailureHandler)
                  }
                }
              }
            } catch {
              case NonFatal(e) =>
                return recover(e)
            }
            shiftDsl.cpsApply(protectedContinuation, recover)
          }

        }

      }
    }

}
