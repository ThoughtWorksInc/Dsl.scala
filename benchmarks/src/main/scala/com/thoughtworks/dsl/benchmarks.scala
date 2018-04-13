package com.thoughtworks.dsl

import java.util.concurrent.{ExecutorService, Executors}

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.keywords.{Each, Shift}

import scala.util.{Success, Try}
import com.thoughtworks.dsl.task.{Task, _}
import com.thoughtworks.dsl.keywords.Shift.implicitShift
import monix.execution.{Cancelable, Scheduler}
import org.openjdk.jmh.annotations.{Fork => JmhFork, _}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Promise, SyncVar}
import scala.util.control.NoStackTrace

object benchmarks {

  private def blockingExecuteMonix[A](task: _root_.monix.eval.Task[A])(
      implicit executionContext: ExecutionContext): A = {
    val syncVar = new SyncVar[Try[A]]
    task.runOnComplete(syncVar.put)(Scheduler(executionContext))
    syncVar.take.get
  }

  private def blockingAwaitMonix[A](task: _root_.monix.eval.Task[A]): A = {
    val syncVar = new SyncVar[Try[A]]
    task.runOnComplete(syncVar.put)(Scheduler.trampoline())
    syncVar.take.get
  }

  final class IntException(val n: Int) extends Exception with NoStackTrace

  @State(Scope.Benchmark)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  @JmhFork(value = 1)
  abstract class BenchmarkState {

    @Param(Array("100"))
    var totalLoops: Int = _

  }

  abstract class FlatTraverse extends BenchmarkState {

    implicit lazy val threadPool = {
      ExecutionContext.fromExecutorService(Executors.newWorkStealingPool())
    }

    @TearDown
    def tearDown() = {
      threadPool.shutdown()
    }

    @Benchmark
    def dsl() = {
      import task.Task

      def cellTask(x: Int, y: Int): Task[List[Int]] = _ {
        !Task.switchExecutionContext(threadPool)
        if (math.abs(x - y) < 5) {
          List(x, y)
        } else {
          List.empty
        }
      }

      def listTask: Task[List[Int]] = {
        val x = !Each(0 until totalLoops)
        val y = !Each(0 until totalLoops)
        cellTask(x, y)
      }

      def sumTask: Task[Int] = _ {
        (!listTask).sum
      }

      Task.blockingAwait(sumTask)
    }

    @Benchmark
    def monix() = {
      import _root_.monix.eval.Task

      def cellTask(x: Int, y: Int): Task[List[Int]] =
        Task.fork(Task.delay {
          if (math.abs(x - y) < 5) {
            List(x, y)
          } else {
            List.empty
          }
        })

      def listTask: Task[List[Int]] =
        Task
          .sequence {
            (for {
              x <- 0 until totalLoops
              y <- 0 until totalLoops
            } yield cellTask(x, y))(collection.breakOut(List.canBuildFrom))
          }
          .map(_.flatten)

      def sumTask = listTask.map(_.sum)

      blockingExecuteMonix(sumTask)(threadPool)
    }

    @Benchmark
    def cats() = {
      import _root_.cats.syntax.all._
      import _root_.cats.effect.IO
      import _root_.cats.instances.list._

      def cellTask(x: Int, y: Int): IO[List[Int]] = {
        IO.shift(threadPool).map { _: Unit =>
          if (math.abs(x - y) < 5) {
            List(x, y)
          } else {
            List.empty
          }
        }
      }

      def listTask: IO[List[Int]] = {
        (0 until totalLoops).toList.flatTraverse { x =>
          (0 until totalLoops).toList.flatTraverse { y =>
            cellTask(x, y)
          }
        }

      }

      def sumTask = listTask.map(_.sum)

      sumTask.unsafeRunSync
    }

    @Benchmark
    def scalaz() = {
      import _root_.scalaz.syntax.all._
      import _root_.scalaz.concurrent.Task
      import _root_.scalaz.std.list._

      def cellTask(x: Int, y: Int): Task[List[Int]] =
        Task {
          if (math.abs(x - y) < 5) {
            List(x, y)
          } else {
            List.empty
          }
        }

      def listTask: Task[List[Int]] = {
        (0 until totalLoops).toList.traverseM { x =>
          (0 until totalLoops).toList.traverseM { y =>
            cellTask(x, y)
          }
        }

      }

      def sumTask = listTask.map(_.sum)

      sumTask.unsafePerformSync
    }

    @Benchmark
    def scalaConcurrentFuture(): Unit = {
      import scala.concurrent.Future

      def cellTask(x: Int, y: Int): Future[List[Int]] =
        Future {
          if (math.abs(x - y) < 5) {
            List(x, y)
          } else {
            List.empty
          }
        }

      def listTask: Future[List[Int]] =
        Future
          .sequence {
            (for {
              x <- 0 until totalLoops
              y <- 0 until totalLoops
            } yield cellTask(x, y))(collection.breakOut(List.canBuildFrom))
          }
          .map(_.flatten)

      def sumTask = listTask.map(_.sum)

      Await.result(sumTask, Duration.Inf)

    }
  }

  @Threads(value = Threads.MAX)
  class MultiThreadFlatTraverse extends FlatTraverse

  @Threads(value = 1)
  class SingleThreadFlatTraverse extends FlatTraverse

  abstract class NonTailRecursion extends BenchmarkState {

    @Benchmark
    def dsl(): Unit = {
      def loop(i: Int = 0): task.Task[Int] = _ {
        if (i < totalLoops) {
          !loop(i + 1) + 1
        } else {
          0
        }
      }
      val result = Task.blockingAwait(loop())
      assert(result == totalLoops)
    }

    @Benchmark
    def monix(): Unit = {

      def loop(i: Int = 0): _root_.monix.eval.Task[Int] = {
        if (i < totalLoops) {
          loop(i + 1).map(_ + 1)
        } else {
          _root_.monix.eval.Task.now(0)
        }
      }

      val result = blockingAwaitMonix(loop())
      assert(result == totalLoops)
    }

    @Benchmark
    def scalaz(): Unit = {

      def loop(i: Int = 0): _root_.scalaz.concurrent.Task[Int] = {
        if (i < totalLoops) {
          loop(i + 1).map(_ + 1)
        } else {
          _root_.scalaz.concurrent.Task.now(0)
        }
      }

      val result = loop().unsafePerformSync
      assert(result == totalLoops)
    }

    @Benchmark
    def cats(): Unit = {
      def loop(i: Int = 0): _root_.cats.effect.IO[Int] = {
        if (i < totalLoops) {
          loop(i + 1).map(_ + 1)
        } else {
          _root_.cats.effect.IO.pure(0)
        }
      }
      val result = loop().unsafeRunSync()
      assert(result == totalLoops)
    }

  }

  @Threads(value = Threads.MAX)
  class MultiThreadNonTailRecursion extends NonTailRecursion

  @Threads(value = 1)
  class SingleThreadNonTailRecursion extends NonTailRecursion

  abstract class TailRecursion extends BenchmarkState {

    @Benchmark
    def dsl(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): task.Task[Int] = _ {
        if (i < totalLoops) {
          !loop(i + 1, accumulator + 1)
        } else {
          accumulator
        }
      }

      val result = Task.blockingAwait(loop())
      assert(result == totalLoops)
    }

    @Benchmark
    def monix(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): _root_.monix.eval.Task[Int] = {
        if (i < totalLoops) {
          _root_.monix.eval.Task.suspend(
            loop(i + 1, accumulator + 1)
          )
        } else {
          _root_.monix.eval.Task.now(accumulator)
        }
      }

      val result = blockingAwaitMonix(loop())
      assert(result == totalLoops)

    }

    @Benchmark
    def scalaz(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): _root_.scalaz.concurrent.Task[Int] = {
        if (i < totalLoops) {
          _root_.scalaz.concurrent.Task.suspend(
            loop(i + 1, accumulator + 1)
          )
        } else {
          _root_.scalaz.concurrent.Task.now(accumulator)
        }
      }

      val result = loop().unsafePerformSync
      assert(result == totalLoops)
    }

    @Benchmark
    def cats(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): _root_.cats.effect.IO[Int] = {
        if (i < totalLoops) {
          _root_.cats.effect.IO.suspend(
            loop(i + 1, accumulator + 1)
          )
        } else {
          _root_.cats.effect.IO.pure(accumulator)
        }
      }

      val result = loop().unsafeRunSync()
      assert(result == totalLoops)
    }

  }

  @Threads(value = Threads.MAX)
  class MultiThreadTailRecursion extends TailRecursion

  @Threads(value = 1)
  class SingleThreadTailRecursion extends TailRecursion

  abstract class ExceptionHandling extends BenchmarkState {

    private def error(i: Int): Unit = {
      throw new IntException(i)
    }

    @Benchmark
    def dsl(): Unit = {
      def throwing(i: Int): task.Task[Unit] = _ {
        error(i)
      }
      val tasks: Seq[task.Task[Unit]] = (0 until totalLoops).map { _ =>
        throwing(1)
      }

      def loop(i: Int = 0, accumulator: Int = 0): task.Task[Int] = _ {
        if (i < totalLoops) {
          val n = try {
            !tasks(i)
            i
          } catch {
            case e: IntException =>
              e.n
          }
          !loop(i + 1, accumulator + n)
        } else {
          accumulator
        }
      }

      val result = Task.blockingAwait(loop())
      assert(result == totalLoops)
    }

    @Benchmark
    def monix(): Unit = {
      def throwing(i: Int): _root_.monix.eval.Task[Unit] = _root_.monix.eval.Task.delay {
        error(i)
      }

      val tasks: Seq[_root_.monix.eval.Task[Unit]] = (0 until totalLoops).map { _ =>
        throwing(1)
      }

      def loop(i: Int = 0, accumulator: Int = 0): _root_.monix.eval.Task[Int] = {
        if (i < totalLoops) {
          tasks(i)
            .map(Function.const(i))
            .onErrorRecover {
              case e: IntException =>
                e.n
            }
            .flatMap { n =>
              loop(i + 1, accumulator + n)
            }
        } else {
          _root_.monix.eval.Task.now(accumulator)
        }
      }

      val result = blockingAwaitMonix(loop())
      assert(result == totalLoops)
    }
    @Benchmark
    def scalaz(): Unit = {
      def throwing(i: Int): _root_.scalaz.concurrent.Task[Unit] = _root_.scalaz.concurrent.Task.delay {
        error(i)
      }

      val tasks: Seq[_root_.scalaz.concurrent.Task[Unit]] = (0 until totalLoops).map { _ =>
        throwing(1)
      }

      def loop(i: Int = 0, accumulator: Int = 0): _root_.scalaz.concurrent.Task[Int] = {
        if (i < totalLoops) {
          tasks(i)
            .map(Function.const(i))
            .handle {
              case e: IntException =>
                e.n
            }
            .flatMap { n =>
              loop(i + 1, accumulator + n)
            }
        } else {
          _root_.scalaz.concurrent.Task.now(accumulator)
        }
      }

      val result = loop().unsafePerformSync
      assert(result == totalLoops)
    }
    @Benchmark
    def cats(): Unit = {
      def throwing(i: Int): _root_.cats.effect.IO[Unit] = _root_.cats.effect.IO {
        error(i)
      }

      val tasks: Seq[_root_.cats.effect.IO[Unit]] = (0 until totalLoops).map { _ =>
        throwing(1)
      }

      def loop(i: Int = 0, accumulator: Int = 0): _root_.cats.effect.IO[Int] = {
        if (i < totalLoops) {
          import _root_.cats.syntax.all._
          tasks(i)
            .map(Function.const(i))
            .handleError {
              case e: IntException =>
                e.n
            }
            .flatMap { n =>
              loop(i + 1, accumulator + n)
            }
        } else {
          _root_.cats.effect.IO.pure(accumulator)
        }
      }

      val result = loop().unsafeRunSync
      assert(result == totalLoops)
    }

  }
  @Threads(value = Threads.MAX)
  class MultiThreadExceptionHandling extends ExceptionHandling

  @Threads(value = 1)
  class SingleThreadExceptionHandling extends ExceptionHandling

  abstract class AsyncTask extends BenchmarkState {

    lazy val threadPool = {
      ExecutionContext.fromExecutorService(Executors.newWorkStealingPool())
    }

    @TearDown
    def tearDown() = {
      threadPool.shutdown()
    }

    @Benchmark
    def dsl(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): task.Task[Int] = _ {
        if (i < totalLoops) {
          !Task.switchExecutionContext(threadPool)
          !loop(i + 1, accumulator + 1)
        } else {
          !Task.switchExecutionContext(threadPool)
          accumulator
        }
      }

      val result = Task.blockingAwait(loop())
      assert(result == totalLoops)
    }
    @Benchmark
    def monix(): Unit = {

      def loop(i: Int = 0, accumulator: Int = 0): _root_.monix.eval.Task[Int] = {
        if (i < totalLoops) {
          // suspend is required to avoid stack overflow
          _root_.monix.eval.Task.fork(_root_.monix.eval.Task.suspend(loop(i + 1, accumulator + 1)))
        } else {
          _root_.monix.eval.Task.fork(_root_.monix.eval.Task(accumulator))
        }
      }

      val result = blockingExecuteMonix(loop())(threadPool)
      assert(result == totalLoops)
    }

    @Benchmark
    def scalaz(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): _root_.scalaz.concurrent.Task[Int] = {
        if (i < totalLoops) {
          _root_.scalaz.concurrent.Task.fork(loop(i + 1, accumulator + 1))
        } else {
          _root_.scalaz.concurrent.Task(accumulator)(threadPool)
        }
      }
      val result = loop().unsafePerformSync
      assert(result == totalLoops)
    }

    @Benchmark
    def cats(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): _root_.cats.effect.IO[Int] = {
        if (i < totalLoops) {
          _root_.cats.effect.IO
            .shift(threadPool)
            .flatMap { _: Unit =>
              loop(i + 1, accumulator + 1)
            }
        } else {
          _root_.cats.effect.IO
            .shift(threadPool)
            .map { _: Unit =>
              accumulator
            }
        }
      }
      val result = loop().unsafeRunSync()
      assert(result == totalLoops)
    }

    @Benchmark
    def scalaConcurrentFuture(): Unit = {
      def loop(i: Int = 0, accumulator: Int = 0): _root_.scala.concurrent.Future[Int] = {
        if (i < totalLoops) {
          _root_.scala.concurrent
            .Future(())(threadPool)
            .flatMap { _: Unit =>
              loop(i + 1, accumulator + 1)
            }(threadPool)
        } else {
          _root_.scala.concurrent.Future(accumulator)(threadPool)
        }
      }
      val result = Await.result(loop(), Duration.Inf)
      assert(result == totalLoops)
    }
  }

  @Threads(value = Threads.MAX)
  class MultiThreadAsyncTask extends AsyncTask

  @Threads(value = 1)
  class SingleThreadAsyncTask extends AsyncTask

}
