package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl

import scala.util.control.Exception.Catcher

/** The state for DSL in exception-handling domain.
  *
  * @author 杨博 (Yang Bo)
  */
trait ExceptionHandling[OtherDomain] { self =>
  def onFailure(handler: Throwable => OtherDomain): OtherDomain

}

object ExceptionHandling {

  implicit final class CpsCatchOps[OtherDomain](catcher: Catcher[ExceptionHandling[OtherDomain]]) {
    def cpsCatch(
        continuation: (
            ExceptionHandling[OtherDomain] => ExceptionHandling[OtherDomain]) => ExceptionHandling[OtherDomain])
      : ExceptionHandling[OtherDomain] = {
      new ExceptionHandling[OtherDomain] {
        def onFailure(failureHandler: Throwable => OtherDomain): OtherDomain = {
          def handleRethrow(e: Throwable): OtherDomain = {
            locally {
              try {
                catcher.lift(e)
              } catch {
                case rethrown: Throwable =>
                  return failureHandler(rethrown)
              }
            } match {
              case Some(handled) => handled.onFailure(failureHandler)
              case None          => failureHandler(e)
            }
          }

          locally {
            try {
              continuation { domain =>
                ExceptionHandling.success(domain.onFailure(failureHandler))
              }
            } catch {
              case e: Throwable => return handleRethrow(e)
            }
          }.onFailure(handleRethrow)
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
                case e: Throwable =>
                  return failureHandler(e)
              }).onFailure(failureHandler)

            restDsl.interpret(instruction, restHandler)
          }
        }

    }

}
