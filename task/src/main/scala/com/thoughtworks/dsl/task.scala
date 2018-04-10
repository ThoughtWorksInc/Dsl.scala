package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.{!!, Continuation, reset}
import com.thoughtworks.dsl.keywords.Shift

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future, Promise, SyncVar}
import scala.util.control.{NonFatal, TailCalls}
import scala.util.{Failure, Success, Try}
import scala.util.control.TailCalls.TailRec

/**
  * @author 杨博 (Yang Bo)
  */
object task {

  @inline
  private[dsl] def jvmCatch[Domain](eh: => Domain !! Throwable)(failureHandler: Throwable => Domain)(
      implicit shiftDsl: Dsl[Shift[Domain, Throwable], Domain, Throwable]): Domain = {
    val protectedContinuation: Domain !! Throwable = try {
      eh
    } catch {
      case NonFatal(e) =>
        return failureHandler(e)
    }
    shiftDsl.interpret(protectedContinuation, failureHandler)
  }

  type Task[+A] = TailRec[Unit] !! Throwable !! A

  object Task {

    @inline
    def now[A](a: A): Task[A] = _(a)

    @inline
    def delay[A](f: () => A): Task[A] = _(f())

    @inline
    def reset[A](a: => A): Task[A] @reset = delay(a _)

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
    def join[Element, That](element: Element)(
        implicit canBuildFrom: CanBuildFrom[Nothing, Element, That]): Task[That] @reset = now {
      (canBuildFrom() += element).result()
    }
  }

  implicit final class ThrowableContinuationOps[Domain, A](private[task] val task: Domain !! Throwable !! A)
      extends AnyVal {
    def run: Domain !! Try[A] = { handler =>
      task { a => failureHandler =>
        handler(Success(a))
      } { e =>
        handler(Failure(e))
      }
    }

  }

  implicit final class TaskOps[A](private[task] val task: Task[A]) extends AnyVal {

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

}
