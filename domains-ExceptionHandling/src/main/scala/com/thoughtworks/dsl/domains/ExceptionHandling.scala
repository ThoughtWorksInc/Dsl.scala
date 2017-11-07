package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
trait ExceptionHandling[Domain] { self =>
  def onFailure(handler: Throwable => Domain): Domain

  final def cpsCatch(catcher: Catcher[ExceptionHandling[Domain]]): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def onFailure(failureHandler: Throwable => Domain): Domain = {
      self.onFailure { e =>
        object Extractor {
          def unapply(e: Throwable): Option[ExceptionHandling[Domain]] = catcher.lift(e)
        }

        e match {
          case Extractor(handled) => handled.onFailure(failureHandler)
          case _                  => failureHandler(e)
        }

      }
    }
  }

}

object ExceptionHandling {
  def success[Domain](r: Domain): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def onFailure(handler: Throwable => Domain): Domain = r
  }

  def failure[Domain](e: Throwable): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def onFailure(handler: Throwable => Domain): Domain = handler(e)
  }

  implicit def mayFailCpsApply[Instruction, Domain, A](
      implicit restCps: Dsl[Instruction, Domain, A]): Dsl[Instruction, ExceptionHandling[Domain], A] =
    new Dsl[Instruction, ExceptionHandling[Domain], A] {
      def interpret(continuation: Instruction, successHandler: A => ExceptionHandling[Domain]): ExceptionHandling[Domain] =
        new ExceptionHandling[Domain] {
          def onFailure(failureHandler: Throwable => Domain): Domain = {
            def restHandler(a: A): Domain =
              (try {
                successHandler(a)
              } catch {
                case NonFatal(e) =>
                  return failureHandler(e)
              }).onFailure(failureHandler)

            restCps.interpret(continuation, restHandler)
          }
        }

    }

}
