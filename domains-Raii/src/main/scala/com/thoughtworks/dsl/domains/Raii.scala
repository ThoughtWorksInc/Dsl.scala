package com.thoughtworks.dsl
package domains
import com.thoughtworks.dsl.Dsl.{!!, Trampoline1, reset}
import com.thoughtworks.dsl.instructions._

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import scala.language.implicitConversions
import com.thoughtworks.dsl.instructions.Shift.StackSafeShiftDsl

/**
  * @author 杨博 (Yang Bo)
  */
sealed trait Raii

object Raii {

  final case class RaiiFailure(throwable: Throwable) extends Raii
  trait RaiiSuccess[Domain] extends Raii {
    def continue(): Domain
  }

  @inline private def catchJvmException[Domain](eh: => Domain !! Raii)(failureHandler: Raii => Domain): Domain = {
    val protectedRaii: Domain !! Raii = try {
      eh
    } catch {
      case NonFatal(e) =>
        return failureHandler(RaiiFailure(e))
    }
    protectedRaii.apply(failureHandler)
  }

  implicit def scopeDsl[Domain, A]: Dsl[Scope[Domain !! Raii, A], Domain !! Raii, A] = {
    new Dsl[Scope[Domain !! Raii, A], Domain !! Raii, A] {
      def interpret(instruction: Scope[Domain !! Raii, A], handler: A => Domain !! Raii): Domain !! Raii = {
        (outerScope: Raii => Domain) =>
          catchJvmException(instruction.continuation { a: A => (scopeHandler: Raii => Domain) =>
            scopeHandler(new RaiiSuccess[Domain] {
              def continue(): Domain = catchJvmException(handler(a))(outerScope)
            })
          }) {
            case throwing: RaiiFailure =>
              outerScope(throwing)
            case breaking: RaiiSuccess[Domain] @unchecked =>
              breaking.continue()
          }

      }
    }
  }

  implicit def raiiCatchDsl[Domain]: Dsl[Catch[Domain !! Raii], Domain !! Raii, Unit] = {
    new Dsl[Catch[Domain !! Raii], Domain !! Raii, Unit] {
      def interpret(instruction: Catch[Domain !! Raii], handler: Unit => Domain !! Raii): Domain !! Raii = {
        (outerScope: Raii => Domain) =>
          catchJvmException(handler(())) {
            case RaiiFailure(e) =>
              catchJvmException(instruction.onFailure(e))(outerScope)
            case breaking =>
              outerScope(breaking)
          }

      }
    }
  }

  implicit final class RaiiContinuationOps[Domain, A](task: Domain !! Raii !! A) {
    def toFuture(implicit hangDsl: Dsl[Hang[Domain], Domain, Domain]): Future[A] = {
      val promise = Promise[A]()
      onComplete { tryResult =>
        promise.complete(tryResult)
        hangDsl.interpret(Hang[Domain], identity)
      }
      promise.future
    }

    def onComplete(handler: Try[A] => Domain): Domain = {
      task { a =>
        new (Domain !! Raii) {
          def apply(outerScope: Raii => Domain): Domain = outerScope(
            new RaiiSuccess[Domain] {
              def continue(): Domain = handler(Success(a))
            }
          )
        }
      } {
        case RaiiFailure(e) =>
          handler(Failure(e))
        case returning: RaiiSuccess[Domain] @unchecked =>
          returning.continue()
      }
    }
  }

  implicit def raiiAutoCloseDsl[Domain, R <: AutoCloseable](
      implicit shiftDsl: Dsl[Shift[Domain, Domain !! Raii], Domain, Domain !! Raii])
    : Dsl[AutoClose[R], Domain !! Raii, R] =
    new Dsl[AutoClose[R], Domain !! Raii, R] {
      def interpret(instruction: AutoClose[R], body: R => Domain !! Raii): Domain !! Raii = {
        val AutoClose(open) = instruction
        val r = open()
        (outerScope: Raii => Domain) =>
          catchJvmException {
            body(r)
          } { raii2: Raii =>
            outerScope(try {
              r.close()
              raii2
            } catch {
              case NonFatal(e) =>
                RaiiFailure(e)
            })
          }
      }
    }

  implicit def liftRaiiDsl[Instruction, Domain, A](
      implicit restDsl: Dsl[Instruction, Domain, A]
  ): Dsl[Instruction, Domain !! Raii, A] =
    new Dsl[Instruction, Domain !! Raii, A] {
      def interpret(instruction: Instruction, successHandler: A => Domain !! Raii): Domain !! Raii = {
        (continue: Raii => Domain) =>
          restDsl.interpret(instruction, { a =>
            catchJvmException(successHandler(a))(continue)
          })
      }

    }

  // TODO: Trampoline
  implicit def stackSafeShiftRaiiDsl[Domain, Value]: StackSafeShiftDsl[Domain !! Raii, Value] =
    new StackSafeShiftDsl[Domain !! Raii, Value] {
      def interpret(instruction: Shift[Domain !! Raii, Value], handler: Value => Domain !! Raii): Domain !! Raii = {
        catchJvmException(
          instruction.continuation { value: Value =>
            catchJvmException(
              handler(value)
            )
          }
        )
      }
    }

  type Task[+A] = Unit !! Raii !! A

  object Task {

    def join[Element, That](element: Element)(
        implicit canBuildFrom: CanBuildFrom[Nothing, Element, That]): Task[That] @reset = now {
      (canBuildFrom() += element).result()
    }
    @inline
    def now[A](a: A): Task[A] = _(a)

    @inline
    def delay[A](f: () => A): Task[A] = { continue =>
      continue(f())
    }

    @inline
    implicit def reset[A](a: => A): Task[A] @reset = delay(a _)

  }

  implicit def await[Domain, Value](continuation: Domain !! Value): Shift[Domain, Value] =
    Shift(continuation)

}
