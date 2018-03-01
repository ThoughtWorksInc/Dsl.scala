package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.instructions.Shift.StackSafeShiftDsl
import com.thoughtworks.dsl.instructions._

import scala.annotation.tailrec
import scala.util.Try
import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/** The state for DSL in exception-handling domain.
  *
  * @author 杨博 (Yang Bo)
  */
trait ExceptionHandling[OtherDomain] extends ((Throwable => OtherDomain) => OtherDomain)

object ExceptionHandling {

  abstract class Trampoline[OtherDomain] extends ExceptionHandling[OtherDomain] {
    def step(): ExceptionHandling[OtherDomain]

    @tailrec
    private def last(): ExceptionHandling[OtherDomain] = {
      step() match {
        case trampoline: Trampoline[OtherDomain] =>
          trampoline.last()
        case notTrampoline =>
          notTrampoline
      }
    }

    final def apply(failureHandler: Throwable => OtherDomain): OtherDomain = {
      catchJvmException(last(), failureHandler)
    }
  }

  implicit def stackSafeShiftExceptionHandlingDsl[Domain, Value]: StackSafeShiftDsl[ExceptionHandling[Domain], Value] =
    new StackSafeShiftDsl[ExceptionHandling[Domain], Value] {
      def interpret(instruction: Shift[ExceptionHandling[Domain], Value],
                    handler: Value => ExceptionHandling[Domain]): ExceptionHandling[Domain] = {
        new Trampoline[Domain] {
          def step(): ExceptionHandling[Domain] = instruction.continuation(handler)
        }
      }
    }

  def success[Domain](finalResult: Domain) = new ExceptionHandling[Domain] {
    def apply(failureHandler: Throwable => Domain): Domain = finalResult
  }

  def failure[Domain](e: Throwable) = new ExceptionHandling[Domain] {
    def apply(failureHandler: Throwable => Domain): Domain = failureHandler(e)
  }

  // Used for Yield
  implicit def liftExceptionHandlingDsl[Instruction, OtherDomain, A](
      implicit restDsl: Dsl[Instruction, OtherDomain, A]
  ): Dsl[Instruction, ExceptionHandling[OtherDomain], A] =
    new Dsl[Instruction, ExceptionHandling[OtherDomain], A] {
      def interpret(instruction: Instruction,
                    successHandler: A => ExceptionHandling[OtherDomain]): ExceptionHandling[OtherDomain] =
        new ExceptionHandling[OtherDomain] {
          def apply(failureHandler: Throwable => OtherDomain): OtherDomain = {
            restDsl.interpret(instruction, { a =>
              catchJvmException(successHandler(a), failureHandler)
            })
          }
        }

    }

  @inline def catchJvmException[OtherDomain](eh: => ExceptionHandling[OtherDomain],
                                             failureHandler: Throwable => OtherDomain): OtherDomain = {
    (try {
      eh
    } catch {
      case e: Throwable =>
        return failureHandler(e)
    }).apply(failureHandler)
  }

  implicit def scopeDsl[Instruction, OtherDomain, A]
    : Dsl[Scope[ExceptionHandling[OtherDomain], A], ExceptionHandling[OtherDomain], A] =
    new Dsl[Scope[ExceptionHandling[OtherDomain], A], ExceptionHandling[OtherDomain], A] {
      def interpret(instruction: Scope[ExceptionHandling[OtherDomain], A],
                    successHandler: A => ExceptionHandling[OtherDomain]): ExceptionHandling[OtherDomain] =
        new ExceptionHandling[OtherDomain] {

          def apply(outerFailureHandler: Throwable => OtherDomain) =
            catchJvmException(
              instruction.continuation { a: A =>
                new ExceptionHandling[OtherDomain] {
                  def apply(innerFailureHandler: Throwable => OtherDomain) = {
                    catchJvmException[OtherDomain](successHandler(a), outerFailureHandler)
                  }
                }
              },
              outerFailureHandler
            )
        }
    }

  implicit def catchDsl[OtherDomain]
    : Dsl[Catch[ExceptionHandling[OtherDomain]], ExceptionHandling[OtherDomain], Unit] = {
    new Dsl[Catch[ExceptionHandling[OtherDomain]], ExceptionHandling[OtherDomain], Unit] {
      def interpret(instruction: Catch[ExceptionHandling[OtherDomain]],
                    body: Unit => ExceptionHandling[OtherDomain]): ExceptionHandling[OtherDomain] = {
        val Catch(recoverer) = instruction
        new ExceptionHandling[OtherDomain] {
          def apply(outerFailureHandler: Throwable => OtherDomain) =
            catchJvmException(body(()), { e: Throwable =>
              catchJvmException(recoverer(e), outerFailureHandler)
            })
        }
      }
    }
  }

}
