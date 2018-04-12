package com.thoughtworks.dsl.keywords

import com.thoughtworks.Extractor._
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Continuation, Keyword}
import com.thoughtworks.dsl.keywords.Catch.CatchDsl

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain, Value](block: Domain !! Value, catcher: Catcher[Domain !! Value])
    extends Keyword[Catch[Domain, Value], Value]
private[keywords] trait LowPriorityCatch0 { this: Catch.type =>

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

  trait CatchDsl[InnerDomain, OuterDomain, Value] extends Dsl[Catch[InnerDomain, Value], OuterDomain, Value] {

    def tryCatch(block: InnerDomain !! Value,
                 catcher: Catcher[InnerDomain !! Value],
                 handler: Value => OuterDomain): OuterDomain

    @inline final def interpret(keyword: Catch[InnerDomain, Value], handler: Value => OuterDomain): OuterDomain = {
      tryCatch(keyword.block, keyword.catcher, handler)
    }
  }

  @inline
  def tryCatch[InnerDomain, OuterDomain, Value](finalizer: Value => OuterDomain)(
      implicit catch2Dsl: CatchDsl[InnerDomain, OuterDomain, Value]) = {
    (block: InnerDomain !! Value, catcher: Catcher[InnerDomain !! Value]) =>
      catch2Dsl.tryCatch(block, catcher, finalizer)
  }

  implicit def throwableCatchDsl[LeftDomain, Value](implicit shiftDsl: Dsl[Shift[LeftDomain, Throwable], LeftDomain, Throwable])
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
              block { value => _ =>
                handler(value)(outerFailureHandler)
              }
            } catch {
              case NonFatal(e) =>
                return recover(e)
            }
            shiftDsl.interpret(protectedContinuation, recover)

          }

        }

      }
    }

}
