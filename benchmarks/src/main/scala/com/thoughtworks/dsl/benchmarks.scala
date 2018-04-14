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
import scala.concurrent._
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

  final val ThreadPool = "thread pool"
  final val CurrentThread = "current thread"

  abstract class SumState extends BenchmarkState {

    @Param(Array("1000"))
    var size: Int = _

    @Param(Array(ThreadPool, CurrentThread))
    var executedIn: String = _

    protected def dslTasks: List[task.Task[Int]] = {
      import task.Task
      {
        executedIn match {
          case ThreadPool =>
            List.fill(size)(_ {
              !Task.switchExecutionContext(threadPool)
              1
            })
          case CurrentThread =>
            List.fill(size)(_(1))
        }
      }
    }

    protected def monixTasks = {
      import _root_.monix.eval.Task
      val scheduler = Scheduler(threadPool: ExecutorService)
      executedIn match {
        case ThreadPool =>
          List.fill(size)(Task.fork(Task.eval(1), scheduler))
        case CurrentThread =>
          List.fill(size)(Task.now(1))
      }
    }

    protected def catsTasks = {
      import _root_.cats.effect.IO
      executedIn match {
        case ThreadPool =>
          List.fill(size) {
            for {
              _ <- IO.shift(threadPool)
            } yield 1
          }
        case CurrentThread =>
          List.fill(size)(IO.pure(1))
      }
    }

    protected def scalazTasks = {
      import _root_.scalaz.concurrent.Task
      executedIn match {
        case ThreadPool =>
          List.fill(size)(Task(1)(threadPool))
        case CurrentThread =>
          List.fill(size)(Task.now(1))
      }
    }

    protected def scalaTasks = {
      import _root_.scala.concurrent.Future
      executedIn match {
        case ThreadPool =>
          List.fill(size)(Future(1)(threadPool))
        case CurrentThread =>
          List.fill(size)(Future.successful(1))
      }
    }

    implicit protected lazy val threadPool = {
      ExecutionContext.fromExecutorService(Executors.newWorkStealingPool())
    }

    @TearDown
    def tearDown() = {
      threadPool.shutdown()
    }

  }

  class CartesianProduct extends BenchmarkState {

    @Param(Array("100"))
    var size: Int = _

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
        val x = !Each(0 until size)
        val y = !Each(0 until size)
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
        for {
          listOfList <- Task
            .sequence {
              (for {
                x <- 0 until size
                y <- 0 until size
              } yield cellTask(x, y))(collection.breakOut(List.canBuildFrom))
            }
        } yield listOfList.flatten

      blockingExecuteMonix(listTask)(threadPool)
    }

    @Benchmark
    def cats() = {
      import _root_.cats.syntax.all._
      import _root_.cats.effect.IO
      import _root_.cats.instances.list._

      def cellTask(x: Int, y: Int): IO[List[Int]] = {
        for {
          _ <- IO.shift(threadPool)
        } yield List(x, y)
      }

      def listTask: IO[List[Int]] = {
        (0 until size).toList.flatTraverse { x =>
          (0 until size).toList.flatTraverse { y =>
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
        (0 until size).toList.traverseM { x =>
          (0 until size).toList.traverseM { y =>
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
        for {
          listOfList <- Future
            .sequence {
              (for {
                x <- 0 until size
                y <- 0 until size
              } yield cellTask(x, y))(collection.breakOut(List.canBuildFrom))
            }
        } yield listOfList.flatten

      Await.result(listTask, Duration.Inf)

    }
  }

  class OptimizedSum extends SumState {

    @Benchmark
    def dsl(): Unit = {
      import com.thoughtworks.dsl.task._

      def loop(tasks: List[Task[Int]], accumulator: Int = 0)(continue: Int => TaskDomain): TaskDomain = {
        tasks match {
          case head :: tail =>
            loop(tail, !head + accumulator)(continue)
          case Nil =>
            continue(accumulator)
        }
      }

      val result = Task.blockingAwait(loop(dslTasks))
      assert(result == size)
    }

    @Benchmark
    def cats(): Unit = {
      import _root_.cats.effect.IO
      def loop(tasks: List[IO[Int]], accumulator: Int = 0): IO[Int] = {
        tasks match {
          case head :: tail =>
            head.flatMap { i =>
              loop(tail, i + accumulator)
            }
          case Nil =>
            IO.pure(accumulator)
        }
      }

      val result = loop(catsTasks).unsafeRunSync()
      assert(result == size)
    }

    @Benchmark
    def monix(): Unit = {
      import _root_.monix.eval.Task

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            head.flatMap { i =>
              loop(tail, i + accumulator)
            }
          case Nil =>
            Task.now(accumulator)
        }
      }

      val result = blockingAwaitMonix(loop(monixTasks))
      assert(result == size)
    }

    @Benchmark
    def scalaz(): Unit = {
      import _root_.scalaz.concurrent.Task

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = {
        tasks match {
          case head :: tail =>
            head.flatMap { i =>
              loop(tail, i + accumulator)
            }
          case Nil =>
            Task.now(accumulator)
        }
      }

      val result = loop(scalazTasks).unsafePerformSync
      assert(result == size)
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.concurrent.Future

      def loop(tasks: List[Future[Int]], accumulator: Int = 0): Future[Int] = {
        tasks match {
          case head :: tail =>
            head.flatMap { i =>
              loop(tail, i + accumulator)
            }
          case Nil =>
            Future.successful(accumulator)
        }
      }

      val result = Await.result(loop(scalaTasks), Duration.Inf)
      assert(result == size)
    }

  }

  class LeftAssociatedSum extends SumState {

    @Benchmark
    def dsl(): Unit = {
      import com.thoughtworks.dsl.task._

      def loop(tasks: List[Task[Int]]): Task[Int] = _ {
        tasks match {
          case head :: tail =>
            !head + !loop(tail)
          case Nil =>
            0
        }
      }
      val result = Task.blockingAwait(loop(dslTasks))
      assert(result == size)
    }

    @Benchmark
    def monix(): Unit = {
      import _root_.monix.eval.Task

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

      val result = blockingAwaitMonix(loop(monixTasks))
      assert(result == size)
    }

    @Benchmark
    def scalaz(): Unit = {
      import _root_.scalaz.concurrent.Task

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

      val result = loop(scalazTasks).unsafePerformSync
      assert(result == size)
    }

    @Benchmark
    def cats(): Unit = {
      import _root_.cats.effect.IO

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

      val result = loop(catsTasks).unsafeRunSync()
      assert(result == size)
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.concurrent.Future

      def loop(tasks: List[Future[Int]]): Future[Int] = {
        tasks match {
          case head :: tail =>
            for {
              i <- head
              accumulator <- loop(tail)
            } yield i + accumulator
          case Nil =>
            Future.successful(0)
        }
      }

      val result = Await.result(loop(scalaTasks), Duration.Inf)
      assert(result == size)
    }

  }

  class RightAssociatedSum extends SumState {

    @Benchmark
    def dsl(): Unit = {
      import com.thoughtworks.dsl.task._

      def loop(tasks: List[Task[Int]], accumulator: Int = 0): Task[Int] = _ {
        tasks match {
          case head :: tail =>
            !loop(tail, !head + accumulator)
          case Nil =>
            accumulator
        }
      }

      val result = Task.blockingAwait(loop(dslTasks))
      assert(result == size)
    }

    @Benchmark
    def cats(): Unit = {
      import _root_.cats.effect.IO

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

      val result = loop(catsTasks).unsafeRunSync()
      assert(result == size)
    }

    @Benchmark
    def monix(): Unit = {
      import _root_.monix.eval.Task

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

      val result = blockingAwaitMonix(loop(monixTasks))
      assert(result == size)
    }

    @Benchmark
    def scalaz(): Unit = {
      import _root_.scalaz.concurrent.Task

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

      val result = loop(scalazTasks).unsafePerformSync
      assert(result == size)
    }

    @Benchmark
    def scala(): Unit = {
      import _root_.scala.concurrent.Future

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

      val result = Await.result(loop(scalaTasks), Duration.Inf)
      assert(result == size)
    }

  }

}
