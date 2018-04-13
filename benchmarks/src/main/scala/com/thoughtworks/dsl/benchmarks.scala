package com.thoughtworks.dsl

import java.util.concurrent.{ExecutorService, Executors}

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.keywords.{Each, Shift}

import scala.util.{Success, Try}
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
  @JmhFork(value = 1, jvmArgsAppend = Array("-XX:+EliminateAllocations", "-XX:+DoEscapeAnalysis"))
  abstract class BenchmarkState

  abstract class SumState extends BenchmarkState {

    @Param(Array("1000"))
    var listSize: Int = _

  }

  abstract class AsynchronousCartesianProduct extends BenchmarkState {

    @Param(Array("100"))
    var listSize: Int = _

    implicit lazy val threadPool = {
      ExecutionContext.fromExecutorService(Executors.newWorkStealingPool())
    }

    @TearDown
    def tearDown() = {
      threadPool.shutdown()
    }

    @Benchmark
    def dsl() = {
      import com.thoughtworks.dsl.task._

      def cellTask(x: Int, y: Int): Task[List[Int]] = _ {
        !Task.switchExecutionContext(threadPool)
        List(x, y)
      }

      def listTask: Task[List[Int]] = {
        val x = !Each(0 until listSize)
        val y = !Each(0 until listSize)
        cellTask(x, y)
      }

      Task.blockingAwait(listTask)
    }

    @Benchmark
    def monix() = {
      import _root_.monix.eval.Task

      def cellTask(x: Int, y: Int): Task[List[Int]] =
        Task.fork(Task.delay {
          List(x, y)
        })

      def listTask: Task[List[Int]] =
        Task
          .sequence {
            (for {
              x <- 0 until listSize
              y <- 0 until listSize
            } yield cellTask(x, y))(collection.breakOut(List.canBuildFrom))
          }
          .map(_.flatten)

      blockingExecuteMonix(listTask)(threadPool)
    }

    @Benchmark
    def cats() = {
      import _root_.cats.syntax.all._
      import _root_.cats.effect.IO
      import _root_.cats.instances.list._

      def cellTask(x: Int, y: Int): IO[List[Int]] = {
        IO.shift(threadPool).map { _: Unit =>
          List(x, y)
        }
      }

      def listTask: IO[List[Int]] = {
        (0 until listSize).toList.flatTraverse { x =>
          (0 until listSize).toList.flatTraverse { y =>
            cellTask(x, y)
          }
        }

      }

      listTask.unsafeRunSync
    }

    @Benchmark
    def scalaz() = {
      import _root_.scalaz.syntax.all._
      import _root_.scalaz.concurrent.Task
      import _root_.scalaz.std.list._

      def cellTask(x: Int, y: Int): Task[List[Int]] =
        Task {
          List(x, y)
        }

      def listTask: Task[List[Int]] = {
        (0 until listSize).toList.traverseM { x =>
          (0 until listSize).toList.traverseM { y =>
            cellTask(x, y)
          }
        }

      }

      listTask.unsafePerformSync
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.concurrent.Future

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
              x <- 0 until listSize
              y <- 0 until listSize
            } yield cellTask(x, y))(collection.breakOut(List.canBuildFrom))
          }
          .map(_.flatten)

      Await.result(listTask, Duration.Inf)

    }
  }

  @Threads(value = Threads.MAX)
  class MultiThreadAsynchronousCartesianProduct extends AsynchronousCartesianProduct

  @Threads(value = 1)
  class SingleThreadAsynchronousCartesianProduct extends AsynchronousCartesianProduct

  abstract class NonTailRecursion extends SumState {

    @Benchmark
    def dsl(): Unit = {
      import com.thoughtworks.dsl.task._

      val tasks: List[Task[Int]] = {
        List.fill(listSize)(_(1))
      }

      def loop(tasks: List[Task[Int]]): Task[Int] = _ {
        tasks match {
          case head :: tail =>
            !head + !loop(tail)
          case Nil =>
            0
        }
      }
      val result = Task.blockingAwait(loop(tasks))
      assert(result == listSize)
    }

    @Benchmark
    def monix(): Unit = {
      import _root_.monix.eval.Task

      val tasks: List[Task[Int]] = {
        List.fill(listSize)(Task.eval(1))
      }

      def loop(tasks: List[Task[Int]]): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              accumulator <- loop(tail)
            } yield i + accumulator
          case Nil =>
            Task(0)
        }
      }

      val result = blockingAwaitMonix(loop(tasks))
      assert(result == listSize)
    }

    @Benchmark
    def scalaz(): Unit = {
      import _root_.scalaz.concurrent.Task
      val tasks: List[Task[Int]] = {
        List.fill(listSize)(Task.delay(1))
      }

      def loop(tasks: List[Task[Int]]): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              accumulator <- loop(tail)
            } yield i + accumulator
          case Nil =>
            Task(0)
        }
      }

      val result = loop(tasks).unsafePerformSync
      assert(result == listSize)
    }

    @Benchmark
    def cats(): Unit = {
      import _root_.cats.effect.IO

      val tasks: List[IO[Int]] = {
        List.fill(listSize)(IO(1))
      }

      def loop(tasks: List[IO[Int]]): IO[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              accumulator <- loop(tail)
            } yield i + accumulator
          case Nil =>
            IO.pure(0)
        }
      }

      val result = loop(tasks).unsafeRunSync()
      assert(result == listSize)
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.util.control.TailCalls, TailCalls.TailRec
      val tasks: List[TailRec[Int]] = {
        List.fill(listSize)(for {
          _ <- TailCalls.done(())
        } yield 1)
      }

      def loop(tasks: List[TailRec[Int]]): TailRec[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              accumulator <- loop(tail)
            } yield i + accumulator
          case Nil =>
            TailCalls.done(0)
        }
      }

      val result = loop(tasks).result
      assert(result == listSize)
    }

  }

  @Threads(value = Threads.MAX)
  class MultiThreadNonTailRecursion extends NonTailRecursion

  @Threads(value = 1)
  class SingleThreadNonTailRecursion extends NonTailRecursion

  abstract class TailRecursion extends SumState {

    @Benchmark
    def dsl(): Unit = {
      import com.thoughtworks.dsl.task._

      val tasks: List[Task[Int]] = {
        List.fill(listSize)(_(1))
      }
      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = _ {
        tasks match {
          case head :: tail =>
            !loop(tail, !head + accumulator)
          case Nil =>
            accumulator
        }
      }

      val result = Task.blockingAwait(loop(tasks))
      assert(result == listSize)
    }

    @Benchmark
    def cats(): Unit = {
      import _root_.cats.effect.IO
      val tasks = {
        List.fill(listSize)(IO(1))
      }
      def loop(tasks: List[IO[Int]], accumulator: Int = 0): IO[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            IO.pure(accumulator)
        }
      }

      val result = loop(tasks).unsafeRunSync()
      assert(result == listSize)
    }

    @Benchmark
    def monix(): Unit = {
      import _root_.monix.eval.Task

      val tasks = List.fill(listSize)(Task.eval(1))

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            Task.now(accumulator)
        }
      }

      val result = blockingAwaitMonix(loop(tasks))
      assert(result == listSize)
    }

    @Benchmark
    def scalaz(): Unit = {
      import _root_.scalaz.concurrent.Task
      val tasks = List.fill(listSize)(Task.delay(1))

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            Task.now(accumulator)
        }
      }

      val result = loop(tasks).unsafePerformSync
      assert(result == listSize)
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.util.control.TailCalls, TailCalls.TailRec
      val tasks: List[TailRec[Int]] = {
        List.fill(listSize)(for {
          _ <- TailCalls.done(())
        } yield 1)
      }

      def loop(tasks: List[TailRec[Int]], accumulator: Int = 0): TailRec[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            TailCalls.done(accumulator)
        }
      }

      val result = loop(tasks).result
      assert(result == listSize)
    }

  }

  @Threads(value = Threads.MAX)
  class MultiThreadTailRecursion extends TailRecursion

  @Threads(value = 1)
  class SingleThreadTailRecursion extends TailRecursion

  abstract class ExceptionHandling extends SumState {

    private def error(i: Int): Int = {
      throw new IntException(i)
    }

    @Benchmark
    def dsl(): Unit = {
      import com.thoughtworks.dsl.task._

      val tasks: List[Task[Int]] = {
        List.fill(listSize)(_(error(1)))
      }

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = _ {
        tasks match {
          case head :: tail =>
            val i = try {
              !head
            } catch {
              case e: IntException =>
                e.n
            }
            !loop(tail, i + accumulator)
          case Nil =>
            accumulator
        }
      }

      val result = Task.blockingAwait(loop(tasks))
      assert(result == listSize)
    }

    @Benchmark
    def monix(): Unit = {
      import _root_.monix.eval.Task

      val tasks: List[Task[Int]] = {
        List.fill(listSize)(Task.eval(error(1)))
      }
      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head.onErrorRecover {
                case e: IntException =>
                  e.n
              }
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            Task.now(accumulator)
        }
      }
      val result = blockingAwaitMonix(loop(tasks))
      assert(result == listSize)
    }

    @Benchmark
    def scalaz(): Unit = {
      import _root_.scalaz.concurrent.Task

      val tasks: List[Task[Int]] = {
        List.fill(listSize)(Task.delay(error(1)))
      }
      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head.handle {
                case e: IntException =>
                  e.n
              }
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            Task.now(accumulator)
        }
      }
      val result = loop(tasks).unsafePerformSync
      assert(result == listSize)

    }

    @Benchmark
    def cats(): Unit = {
      import _root_.cats.effect.IO

      val tasks: List[IO[Int]] = {
        List.fill(listSize)(IO(error(1)))
      }
      def loop(tasks: List[IO[Int]], accumulator: Int = 0): IO[Int] = {
        tasks match {
          case head :: tail =>
            import _root_.cats.syntax.all._
            for {
              i <- head.handleError {
                case e: IntException =>
                  e.n
              }
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            IO.pure(accumulator)
        }
      }
      val result = loop(tasks).unsafeRunSync()
      assert(result == listSize)
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.util.control.TailCalls, TailCalls.TailRec
      val tasks: List[TailRec[Int]] = {
        List.fill(listSize)(TailCalls.done(()).map(Function.const(error(1))))
      }

      def loop(tasks: List[TailRec[Int]], accumulator: Int = 0): TailRec[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- {
                for {
                  _ <- TailCalls.done(())
                } yield {
                  try {
                    head.result
                  } catch {
                    case e: IntException =>
                      e.n
                  }
                }
              }
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            TailCalls.done(accumulator)
        }
      }

      val result = loop(tasks).result
      assert(result == listSize)

    }

  }
  @Threads(value = Threads.MAX)
  class MultiThreadExceptionHandling extends ExceptionHandling

  @Threads(value = 1)
  class SingleThreadExceptionHandling extends ExceptionHandling

  abstract class Asynchronous extends SumState {

    implicit private lazy val threadPool = {
      ExecutionContext.fromExecutorService(Executors.newWorkStealingPool())
    }

    @TearDown
    def tearDown() = {
      threadPool.shutdown()
    }

    @Benchmark
    def dsl(): Unit = {
      import com.thoughtworks.dsl.task._

      val tasks: List[Task[Int]] = {
        List.fill(listSize)(_ {
          !Task.switchExecutionContext(threadPool)
          1
        })
      }

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = _ {
        tasks match {
          case head :: tail =>
            !loop(tail, !head + accumulator)
          case Nil =>
            accumulator
        }
      }

      val result = Task.blockingAwait(loop(tasks))
      assert(result == listSize)
    }

    @Benchmark
    def cats(): Unit = {
      import _root_.cats.effect.IO
      val tasks = {
        List.fill(listSize) {
          for {
            _ <- IO.shift(threadPool)
          } yield 1
        }
      }
      def loop(tasks: List[IO[Int]], accumulator: Int = 0): IO[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            IO.pure(accumulator)
        }
      }

      val result = loop(tasks).unsafeRunSync()
      assert(result == listSize)
    }

    @Benchmark
    def monix(): Unit = {
      import _root_.monix.eval.Task

      val tasks = List.fill(listSize)(Task.fork(Task.eval(1)))

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            Task.now(accumulator)
        }
      }

      val result = blockingExecuteMonix(loop(tasks))(threadPool)
      assert(result == listSize)
    }

    @Benchmark
    def scalaz(): Unit = {
      import _root_.scalaz.concurrent.Task
      val tasks = List.fill(listSize)(Task(1))

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            Task.now(accumulator)
        }
      }

      val result = loop(tasks).unsafePerformSync
      assert(result == listSize)
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.concurrent.Future
      val tasks: List[Future[Int]] = {
        List.fill(listSize)(Future(1))
      }

      def loop(tasks: List[Future[Int]], accumulator: Int = 0): Future[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              r <- loop(tail, i + accumulator)
            } yield r
          case Nil =>
            Future.successful(accumulator)
        }
      }

      val result = Await.result(loop(tasks), Duration.Inf)
      assert(result == listSize)
    }

  }

  @Threads(value = Threads.MAX)
  class MultiThreadAsynchronous extends Asynchronous

  @Threads(value = 1)
  class SingleThreadAsynchronous extends Asynchronous

}
