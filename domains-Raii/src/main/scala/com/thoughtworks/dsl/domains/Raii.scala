package com.thoughtworks.dsl
package domains
import com.thoughtworks.dsl.Dsl.{!!, reset}
import com.thoughtworks.dsl.instructions._

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, Promise, SyncVar}
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
              catchJvmException(instruction.failureHandler(e))(outerScope)
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

    def blockingAwait()(implicit hangDsl: Dsl[Hang[Domain], Domain, Domain]): A = {
      val syncVar = new SyncVar[Try[A]]
      onComplete { result =>
        syncVar.put(result)
        hangDsl.interpret(Hang[Domain], identity)
      }
      syncVar.take.get
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

  abstract class TrampolineContinuation[Domain] extends (Domain !! Raii) {
    def step(): Domain !! Raii

    @tailrec
    @inline
    private final def last(): Domain !! Raii = {
      step() match {
        case trampoline: TrampolineContinuation[Domain] =>
          trampoline.last()
        case notTrampoline =>
          notTrampoline
      }
    }

    def apply(raiiHandler: Raii => Domain): Domain = {
      catchJvmException(last())(raiiHandler)
    }
  }

  implicit def stackSafeShiftRaiiDsl[Domain, Value]: StackSafeShiftDsl[Domain !! Raii, Value] =
    new StackSafeShiftDsl[Domain !! Raii, Value] {
      @inline def interpret(instruction: Shift[Domain !! Raii, Value],
                            handler: Value => Domain !! Raii): Domain !! Raii = {
        new TrampolineContinuation[Domain] {
          def step() = instruction.continuation(handler)
        }
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
    def delay[A](f: () => A): Task[A] = _(f())

    @inline
    implicit def reset[A](a: => A): Task[A] @reset = delay(a _)

  }

  /** Implicit converts from a task to a [[instructions.Shift]].
    *
    * @note This implicit conversion enables the [[Dsl.Instruction#unary_$bang !-notation]]
    *       for continuation-passing style functions in [[Raii]] domain.
    */
  implicit def await[Domain, Value](continuation: Domain !! Raii !! Value): Shift[Domain !! Raii, Value] =
    Shift(continuation)

}
