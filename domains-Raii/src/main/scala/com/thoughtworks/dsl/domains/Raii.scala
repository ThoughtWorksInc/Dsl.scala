package com.thoughtworks.dsl
package domains
import com.thoughtworks.dsl.Dsl.{!!, reset}
import com.thoughtworks.dsl.keywords._

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future, Promise, SyncVar}
import scala.util.{Failure, Success, Try}
import scala.util.control.{NonFatal, TailCalls}
import scala.language.implicitConversions
import com.thoughtworks.dsl.keywords.Shift.StackSafeShiftDsl

import scala.util.control.TailCalls.TailRec

/**
  * @author 杨博 (Yang Bo)
  */
@deprecated(message = "This domain will be removed, in favor of `com.thoughtworks.dsl.task`", since = "1.0.0")
sealed trait Raii

object Raii {

  final case class RaiiFailure(throwable: Throwable) extends Raii
  trait RaiiSuccess[Domain] extends Raii {
    def continue(): Domain
  }

  @inline
  private def jvmCatch[Domain](eh: => Domain !! Raii)(raiiHandler: Raii => Domain)(
      implicit shiftDsl: Dsl[Shift[Domain, Raii], Domain, Raii]): Domain = {
    val protectedRaii: Domain !! Raii = try {
      eh
    } catch {
      case NonFatal(e) =>
        return raiiHandler(RaiiFailure(e))
    }
    shiftDsl.interpret(protectedRaii, raiiHandler)
  }

  implicit def scopeDsl[Domain, A](
      implicit shiftDsl: Dsl[Shift[Domain, Raii], Domain, Raii]): Dsl[Scope[Domain !! Raii, A], Domain !! Raii, A] = {
    new Dsl[Scope[Domain !! Raii, A], Domain !! Raii, A] {
      def interpret(keyword: Scope[Domain !! Raii, A], handler: A => Domain !! Raii): Domain !! Raii = {
        (outerScope: Raii => Domain) =>
          jvmCatch(keyword.continuation { a: A => (scopeHandler: Raii => Domain) =>
            scopeHandler(new RaiiSuccess[Domain] {
              def continue(): Domain = jvmCatch(handler(a))(outerScope)
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

  implicit def raiiCatchDsl[Domain](
      implicit shiftDsl: Dsl[Shift[Domain, Raii], Domain, Raii]): Dsl[Catch[Domain !! Raii], Domain !! Raii, Unit] = {
    new Dsl[Catch[Domain !! Raii], Domain !! Raii, Unit] {
      def interpret(keyword: Catch[Domain !! Raii], handler: Unit => Domain !! Raii): Domain !! Raii = {
        (outerScope: Raii => Domain) =>
          jvmCatch(handler(())) {
            case RaiiFailure(e) =>
              jvmCatch(keyword.failureHandler(e))(outerScope)
            case breaking =>
              outerScope(breaking)
          }

      }
    }
  }

  implicit final class RaiiContinuationOps[Domain, A](task: Domain !! Raii !! A) {
    def run: Domain !! Try[A] = { handler =>
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

  implicit final class TaskOps[A](task: Task[A]) {

    def onComplete: Unit !! Try[A] = { continue =>
      task.run { result =>
        TailCalls.done(continue(result))
      }.result
    }

    def toFuture: Future[A] = {
      val promise = Promise[A]()
      task.run { tryResult =>
        promise.complete(tryResult)
        TailCalls.done(())
      }.result
      promise.future
    }

    def blockingAwait(): A = {
      val syncVar = new SyncVar[Try[A]]
      task.run { result =>
        syncVar.put(result)
        TailCalls.done(())
      }.result
      syncVar.take.get
    }

  }
  implicit def raiiAutoCloseDsl[Domain, R <: AutoCloseable](
      implicit shiftDsl: Dsl[Shift[Domain, Raii], Domain, Raii]): Dsl[AutoClose[R], Domain !! Raii, R] =
    new Dsl[AutoClose[R], Domain !! Raii, R] {
      def interpret(keyword: AutoClose[R], body: R => Domain !! Raii): Domain !! Raii = {
        val AutoClose(open) = keyword
        val r = open()
        (outerScope: Raii => Domain) =>
          jvmCatch {
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

  implicit def liftRaiiDsl[Keyword, Domain, A](
      implicit restDsl: Dsl[Keyword, Domain, A],
      shiftDsl: Dsl[Shift[Domain, Raii], Domain, Raii]
  ): Dsl[Keyword, Domain !! Raii, A] =
    new Dsl[Keyword, Domain !! Raii, A] {
      def interpret(keyword: Keyword, successHandler: A => Domain !! Raii): Domain !! Raii = {
        (continue: Raii => Domain) =>
          restDsl.interpret(keyword, { a =>
            jvmCatch(successHandler(a))(continue)
          })
      }

    }

  final case class TrampolineContinuation[Domain, Value](continuation: Domain !! Raii !! Value,
                                                         handler: Value => Domain !! Raii)
      extends (Domain !! Raii) {

    @tailrec
    @inline
    private def last(): Domain !! Raii = {
      continuation(handler) match {
        case trampoline: TrampolineContinuation[Domain, Value] =>
          trampoline.last()
        case notTrampoline =>
          notTrampoline
      }
    }

    def apply(raiiHandler: Raii => Domain): Domain = {
      jvmCatch(last())(raiiHandler)
    }
  }

  @inline
  implicit def stackSafeShiftRaiiDsl[Domain, Value]: StackSafeShiftDsl[Domain !! Raii, Value] =
    new StackSafeShiftDsl[Domain !! Raii, Value] {
      @inline def interpret(keyword: Shift[Domain !! Raii, Value],
                            handler: Value => Domain !! Raii): Domain !! Raii = {
        TrampolineContinuation(keyword.continuation, handler)
      }
    }

  type Task[+A] = TailRec[Unit] !! Raii !! A

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
    def switchExecutionContext(executionContext: ExecutionContext): Task[Unit] = { continue => raiiHandler =>
      executionContext.execute(new Runnable {
        def run(): Unit = {
          jvmCatch(continue(()))(raiiHandler).result
        }
      })
      TailCalls.done(())
    }

    @inline
    implicit def reset[A](a: => A): Task[A] @reset = delay(a _)

  }

  /** Implicit converts from a task to a [[keywords.Shift]].
    *
    * @note This implicit conversion enables the [[Dsl.Keyword#unary_$bang !-notation]]
    *       for continuation-passing style functions in [[Raii]] domain.
    */
  implicit def await[Domain, Value](continuation: Domain !! Raii !! Value): Shift[Domain !! Raii, Value] =
    Shift(continuation)

}
