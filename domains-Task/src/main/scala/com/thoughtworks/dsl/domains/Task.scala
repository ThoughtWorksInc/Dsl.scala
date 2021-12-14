package com.thoughtworks.dsl
package domains

import com.thoughtworks.dsl.domains.Continuation, Continuation.!!
import com.thoughtworks.dsl.keywords.Shift
import com.thoughtworks.dsl.reset, reset._

import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise, SyncVar}
import scala.util.control.{NonFatal, TailCalls}
import scala.util.{Failure, Success, Try}
import scala.util.control.TailCalls.TailRec


/** The asynchronous task that supports exception handling, resource management, and is stack-safe.
  *
  * @template
  * @example A [[Task]] can be created from `for`-comprehension,
  *          where [[keywords.Each]] and [[keywords.Fork]] can be used together to asynchronously iterate collections.
  *
  *          For example, the above `concatenateRemoteData` downloads and concatenates data from multiple URLs.
  *
  *          {{{
  *          import com.thoughtworks.dsl.Dsl.to
  *          import com.thoughtworks.dsl._
  *          import com.thoughtworks.dsl.keywords._
  *          import com.thoughtworks.dsl.domains.Task
  *          import java.net.URL
  *          def concatenateRemoteData(urls: List[URL], downloader: URL => Task[Vector[Byte]]) = ToView {
  *            for {
  *              url <- Fork(urls)
  *              data <- Shift(downloader(url))
  *              byte <- Each(data)
  *            } yield byte
  *          }.to[Task]
  *          }}}
  *
  *          A [[Task]] can be also created from [[Task.apply]]
  *
  *          {{{
  *          def mockDownloader(url: URL) = Task {
  *            "mock data\n".getBytes.toVector
  *          }
  *          }}}
  *
  *          A [[Task]] can be then converted to [[scala.concurrent.Future]] via [[Task.toFuture]],
  *          in order to integrate into other frameworks.
  *
  *          In this example, it's a `Future[Assertion]` required by [[org.scalatest.freespec.AsyncFreeSpec]].
  *
  *          {{{
  *          val mockUrls = List(new URL("http://example.com/file1"), new URL("http://example.com/file2"))
  *
  *          import org.scalatest.Assertion
  *          def assertion: Task[Assertion] = Task {
  *            new String((!Shift(concatenateRemoteData(mockUrls, mockDownloader))).toArray) should be("mock data\nmock data\n")
  *          }
  *
  *          Task.toFuture(assertion)
  *          }}}
  */
type Task[+A] = Task.TaskDomain !! A

/** @author 杨博 (Yang Bo)
  */
object Task extends TaskPlatformSpecificFunctions {

  type TaskDomain = TailRec[Unit] !! Throwable


  @inline
  def now[A](a: A): Task[A] = _(a)

  @inline
  def delay[A](f: () => A): Task[A] = _(f())

  inline def apply[A](inline a: A): Task[A] = { handler =>
    reset(handler(a))
  }

  /** Returns a task that does nothing but let the succeeding tasks run on `executionContext`
    *
    * @example All the code after a `!switchExecutionContext` should be executed on `executionContext`
    *          {{{
    *          import com.thoughtworks.dsl.reset, reset._
    *          import com.thoughtworks.dsl.domains.Task
    *          import org.scalatest.Assertion
    *          import scala.concurrent.ExecutionContext
    *          import com.thoughtworks.dsl.keywords.Shift
    *          def myTask: Task[Assertion] = *[Task] {
    *            val originalThread = Thread.currentThread
    *            !Shift(Task.switchExecutionContext(ExecutionContext.global))
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
        val protectedContinuation =
          try {
            continue(())
          } catch {
            case NonFatal(e) =>
              return failureHandler(e)
          }
        protectedContinuation(failureHandler)
      }

      def run(): Unit = stackSafeRun().result

    })
    TailCalls.done(())
  }

  @inline
  private def newBuilder[A, C](implicit factory: Factory[A, C]): Builder[A, C] = {
    factory.newBuilder
  }

  def onComplete[A](task: Task[A])(continue: Try[A] => Unit) = {
    Continuation
      .toTryContinuation(task) { result =>
        TailCalls.done(continue(result))
      }
      .result
  }

  /** Converts a [[Task]] to a [[scala.concurrent.Future]].
    *
    * @see [[keywords.Await]] for converting a [[scala.concurrent.Future]] to a [[Task]].
    */
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
