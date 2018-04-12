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
        private def stackSafeRun(): TailRec[Unit] = {

          val protectedContinuation = try {
            continue(())
          } catch {
            case NonFatal(e) =>
              return raiiHandler(e)
          }
          Shift(protectedContinuation).cpsApply(raiiHandler)
        }

        @noinline
        def run(): Unit = stackSafeRun().result

      })
      TailCalls.done(())
    }

    @inline
    def join[Element, That](element: Element)(
        implicit canBuildFrom: CanBuildFrom[Nothing, Element, That]): Task[That] @reset = now {
      (canBuildFrom() += element).result()
    }

    def onComplete[A](task: Task[A])(continue: (Try[A] => Unit)) = {
      Continuation
        .toTryContinuation(task) { result =>
          TailCalls.done(continue(result))
        }
        .result
    }

    def blockingAwait[A](task: Task[A]): A = {
      val syncVar = new SyncVar[Try[A]]
      Continuation
        .toTryContinuation(task) { result =>
          syncVar.put(result)
          TailCalls.done(())
        }
        .result
      syncVar.take.get
    }

    def toFuture[A](task: Task[A]): Future[A] = {
      val promise = Promise[A]()
      Continuation
        .toTryContinuation(task) { tryResult =>
          promise.complete(tryResult)
          TailCalls.done(())
        }
        .result
      promise.future
    }
  }

}
