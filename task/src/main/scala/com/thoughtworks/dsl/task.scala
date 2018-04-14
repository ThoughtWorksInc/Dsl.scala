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

  type TaskDomain = TailRec[Unit] !! Throwable

  type Task[+A] = TaskDomain !! A

  object Task {

    @inline
    def now[A](a: A): Task[A] = _(a)

    @inline
    def delay[A](f: () => A): Task[A] = _(f())

    @inline
    def reset[A](a: => A): Task[A] @reset = delay(a _)

    /** Returns a task that does nothing but let the succeeding tasks run on `executionContext`
      *
      * @example All the code after a `!switchExecutionContext` should be executed on `executionContext`
      *          {{{
      *          import com.thoughtworks.dsl.task.Task
      *          import org.scalatest.Assertion
      *          import scala.concurrent.ExecutionContext
      *          import com.thoughtworks.dsl.keywords.Shift.implicitShift
      *          def myTask: Task[Assertion] = _ {
      *            val originalThread = Thread.currentThread
      *            !Task.switchExecutionContext(ExecutionContext.global)
      *            Thread.currentThread should not be originalThread
      *          }
      *
      *          Task.toFuture(myTask)
      *
      *          }}}
      */
    @inline
    def switchExecutionContext(executionContext: ExecutionContext): Task[Unit] = { continue => failureHandler =>
      executionContext.execute(new Runnable {

        @inline
        private def stackSafeRun(): TailRec[Unit] = {
          val protectedContinuation = try {
            continue(())
          } catch {
            case NonFatal(e) =>
              return failureHandler(e)
          }
          protectedContinuation(failureHandler)
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
