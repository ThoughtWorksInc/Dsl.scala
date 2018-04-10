package com.thoughtworks.dsl.keywords

import com.thoughtworks.Extractor._
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Continuation, Keyword}

import scala.language.implicitConversions
import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
final case class Catch[Domain](failureHandler: Catcher[Domain]) extends AnyVal with Keyword[Catch[Domain], Unit]

private[keywords] trait LowPriorityCatch0 { this: Catch.type =>

  implicit def catchContinuationDsl[Domain, Value](
      implicit restCatchDsl: Dsl[Catch[Domain], Domain, Unit]): Dsl[Catch[Domain !! Value], Domain !! Value, Unit] =
    new Dsl[Catch[Domain !! Value], Domain !! Value, Unit] {

      def interpret(keyword: Catch[Domain !! Value], block: Unit => Domain !! Value): Domain !! Value = {
        (continue: Value => Domain) =>
          restCatchDsl.interpret(
            Catch[Domain] {
              case keyword.failureHandler.extract(combinedDomain) =>
                combinedDomain(continue)
            }, { _: Unit =>
              block(())(continue)
            }
          )
      }
    }

}

object Catch extends LowPriorityCatch0 {

  implicit def implicitCatch[Domain](onFailure: Catcher[Domain]): Catch[Domain] = Catch[Domain](onFailure)

  implicit def throwableCatchDsl[Domain]: Dsl[Catch[Domain !! Throwable], Domain !! Throwable, Unit] =
    new Dsl[Catch[Domain !! Throwable], Domain !! Throwable, Unit] {
      def interpret(keyword: Catch[Domain !! Throwable], handler: Unit => Domain !! Throwable): Domain !! Throwable = {
        finalFailureHandler =>
          @inline
          def jvmCatch(block: => Domain !! Throwable)(failureHandler: Throwable => Domain): Domain = {
            val protectedBlock = try {
              block
            } catch {
              case NonFatal(e) =>
                return failureHandler(e)
            }
            protectedBlock.apply(failureHandler)
          }

          jvmCatch(handler(())) { throwable =>
            jvmCatch(keyword.failureHandler.applyOrElse(throwable, Continuation.now[Domain, Throwable]))(
              finalFailureHandler)
          }

      }
    }

}
