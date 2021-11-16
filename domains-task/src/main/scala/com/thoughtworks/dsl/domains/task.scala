package com.thoughtworks.dsl
package domains

import com.thoughtworks.dsl.Dsl.{!!, Continuation, reset}
import com.thoughtworks.dsl.keywords.Shift
import com.thoughtworks.{enableIf, enableMembersIf}

import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise, SyncVar}
import scala.util.control.{NonFatal, TailCalls}
import scala.util.{Failure, Success, Try}
import scala.util.control.TailCalls.TailRec

/** @author 杨博 (Yang Bo)
  */
object task {

  type TaskDomain = TailRec[Unit] !! Throwable

  /** The asynchronous task that supports exception handling, resource management, and is stack-safe.
    *
    * @template
    * @example A [[Task]] can be created from `for`-comprehension,
    *          where [[keywords.Each]] and [[keywords.Fork]] can be used together to asynchronously iterate collections.
    *
    *          For example, the above `concatenateRemoteData` downloads and concatenates data from multiple URLs.
    *
    *          {{{
    *          import com.thoughtworks.dsl.comprehension._
    *          import com.thoughtworks.dsl.keywords._
    *          import com.thoughtworks.dsl.keywords.Shift._
    *          import com.thoughtworks.dsl.domains.task.Task
    *          import java.net.URL
    *          def concatenateRemoteData(urls: List[URL], downloader: URL => Task[Vector[Byte]]): Task[Vector[Byte]] = {
    *            for {
    *              url <- Fork(urls)
    *              data <- downloader(url)
    *              byte <- Each(data)
    *            } yield byte
    *          }.as[Task[Vector[Byte]]]
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
    *            !concatenateRemoteData(mockUrls, mockDownloader) should be("mock data\nmock data\n".getBytes.toVector)
    *          }
    *
    *          Task.toFuture(assertion)
    *          }}}
    */
  type Task[+A] = TaskDomain !! A

  object Task {

    @inline
    def now[A](a: A): Task[A] = _(a)

    @inline
    def delay[A](f: () => A): Task[A] = _(f())

    @inline
    def apply[A](a: => A): Task[A] @reset = delay(a _)

    /** Returns a task that does nothing but let the succeeding tasks run on `executionContext`
      *
      * @example All the code after a `!switchExecutionContext` should be executed on `executionContext`
      *          {{{
      *          import com.thoughtworks.dsl.domains.task.Task
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
    @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.1(1|2)\..*$"""))
    private[task] object Scala211Or212 {
      type Factory[-A, +C] = scala.collection.generic.CanBuildFrom[Nothing, A, C]

      @inline
      def newBuilder[A, C](implicit factory: Factory[A, C]): Builder[A, C] = {
        factory()
      }

    }

    @enableMembersIf(scala.util.Properties.versionNumberString.matches("""^2\.13\..*$"""))
    private[task] object Scala213 {

      @inline
      def newBuilder[A, C](implicit factory: Factory[A, C]): Builder[A, C] = {
        factory.newBuilder
      }

    }

    import Scala211Or212._
    import Scala213._

    def join[Element, That](element: Element)(implicit factory: Factory[Element, That]): Task[That] @reset = now {
      (newBuilder[Element, That] += element).result()
    }

    def onComplete[A](task: Task[A])(continue: Try[A] => Unit) = {
      Continuation
        .toTryContinuation(task) { result =>
          TailCalls.done(continue(result))
        }
        .result
    }

    @enableIf(c => !c.compilerSettings.exists(_.matches("""^-Xplugin:.*scalajs-compiler_.*\.jar$""")))
    def blockingAwait[A](task: Task[A], timeout: Duration = Duration.Inf): A = {
      val syncVar = new SyncVar[Try[A]]
      Continuation
        .toTryContinuation(task) { result =>
          syncVar.put(result)
          TailCalls.done(())
        }
        .result

      if (timeout.isFinite) {
        syncVar.take(timeout.toMillis).get
      } else {
        syncVar.take.get
      }
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

}
