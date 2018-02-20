package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/** The state for DSL in exception-handling domain.
  *
  * @author 杨博 (Yang Bo)
  */
trait ExceptionHandling[OtherDomain] { self =>
  def onFailure(handler: Throwable => OtherDomain): OtherDomain

}

object ExceptionHandling {

  implicit final class ByNameOps[A, OtherDomain](byName: => A)(
      implicit asExceptionHandling: A <:< ExceptionHandling[OtherDomain]) {

    @inline
    def cpsCatch(catcher: Catcher[ExceptionHandling[OtherDomain]]): ExceptionHandling[OtherDomain] =
      new ExceptionHandling[OtherDomain] {
        def onFailure(failureHandler: Throwable => OtherDomain): OtherDomain = {

          object Extractor {
            def unapply(e: Throwable): Option[ExceptionHandling[OtherDomain]] = catcher.lift(e)
          }
          val byValue: ExceptionHandling[OtherDomain] = try {
            byName
          } catch {
            case Extractor(handled) => return handled.onFailure(failureHandler)
            case NonFatal(e)        => return failureHandler(e)
          }

          byValue.onFailure { e =>
            e match {
              case Extractor(handled) => handled.onFailure(failureHandler)
              case NonFatal(e)        => failureHandler(e)
            }
          }
        }
      }
  }

  def success[Domain](r: Domain): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def onFailure(handler: Throwable => Domain): Domain = r
  }

  def failure[Domain](e: Throwable): ExceptionHandling[Domain] = new ExceptionHandling[Domain] {
    def onFailure(handler: Throwable => Domain): Domain = handler(e)
  }

  implicit def mayFailDsl[Instruction, Domain, A](
      implicit restDsl: Dsl[Instruction, Domain, A]): Dsl[Instruction, ExceptionHandling[Domain], A] =
    new Dsl[Instruction, ExceptionHandling[Domain], A] {
      def interpret(instruction: Instruction,
                    successHandler: A => ExceptionHandling[Domain]): ExceptionHandling[Domain] =
        new ExceptionHandling[Domain] {
          def onFailure(failureHandler: Throwable => Domain): Domain = {
            def restHandler(a: A): Domain =
              (try {
                successHandler(a)
              } catch {
                case NonFatal(e) =>
                  return failureHandler(e)
              }).onFailure(failureHandler)

            restDsl.interpret(instruction, restHandler)
          }
        }

    }

}
