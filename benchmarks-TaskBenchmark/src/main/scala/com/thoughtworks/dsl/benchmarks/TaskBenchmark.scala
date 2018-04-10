package com.thoughtworks.dsl
package benchmarks

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.benchmarks.TaskBenchmark.IntException

import scala.util.{Success, Try}
import com.thoughtworks.dsl.task._
import com.thoughtworks.dsl.keywords.Shift.implicitShift
import monix.execution.{Cancelable, Scheduler}
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, State}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, SyncVar}
import scala.util.control.NoStackTrace
object TaskBenchmark {
  final class IntException(val n: Int) extends Exception with NoStackTrace
}
@State(Scope.Benchmark)
class TaskBenchmark {

  @Benchmark
  def dslStackedCall(): Unit = {
    def loop(i: Int = 0): task.Task[Int] = _ {
      if (i < totalLoops) {
        !loop(i + 1) + i
      } else {
        0
      }
    }

    val result = loop().blockingAwait()
    assert(result == expectedResult)
  }

  @Benchmark
  def dslTailCall(): Unit = {
    def loop(i: Int = 0, accumulator: Int = 0): task.Task[Int] = _ {
      if (i < totalLoops) {
        !loop(i + 1, accumulator + i)
      } else {
        accumulator
      }
    }

    val result = loop().blockingAwait()
    assert(result == expectedResult)
  }

  private def error(i: Int): Unit = {
    throw new IntException(i)
  }

  @Benchmark
  def dslExceptionHandling(): Unit = {
    def throwing(i: Int): task.Task[Unit] = _ {
      error(i)
    }
    val tasks: Seq[task.Task[Unit]] = (0 until totalLoops).map(throwing)

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

    val result = loop().blockingAwait()
    assert(result == expectedResult)
  }

  @Benchmark
  def dslAsyncCall(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    def loop(i: Int = 0, accumulator: Int = 0): task.Task[Int] = _ {
      if (i < totalLoops) {
        !Task.switchExecutionContext(global)
        !loop(i + 1, accumulator + i)
      } else {
        !Task.switchExecutionContext(global)
        accumulator
      }
    }

    val result = loop().blockingAwait()
    assert(result == expectedResult)
  }

  private def blockingAwaitMonix[A](task: monix.eval.Task[A]): A = {
    val syncVar = new SyncVar[Try[A]]
    task.runOnComplete(syncVar.put)(Scheduler.trampoline())
    syncVar.take.get
  }

  @Benchmark
  def monixStackedCall(): Unit = {

    def loop(i: Int = 0): monix.eval.Task[Int] = {
      if (i < totalLoops) {
        loop(i + 1).map(_ + i)
      } else {
        monix.eval.Task(0)
      }
    }

    val result = blockingAwaitMonix(loop())
    assert(result == expectedResult)
  }

  @Benchmark
  def monixExceptionHandling(): Unit = {
    def throwing(i: Int): monix.eval.Task[Unit] = monix.eval.Task {
      error(i)
    }

    val tasks: Seq[monix.eval.Task[Unit]] = (0 until totalLoops).map(throwing)

    def loop(i: Int = 0, accumulator: Int = 0): monix.eval.Task[Int] = {
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
        monix.eval.Task(accumulator)
      }
    }

    val result = blockingAwaitMonix(loop())
    assert(result == expectedResult)
  }

  @Benchmark
  def monixTailCall(): Unit = {
    def loop(i: Int = 0, accumulator: Int = 0): monix.eval.Task[Int] = {
      if (i < totalLoops) {
        monix.eval.Task.suspend(
          loop(i + 1, accumulator + i)
        )
      } else {
        monix.eval.Task(accumulator)
      }
    }

    val result = blockingAwaitMonix(loop())
    assert(result == expectedResult)

  }

  private def blockingExecuteMonix[A](task: monix.eval.Task[A])(implicit executionContext: ExecutionContext): A = {
    val syncVar = new SyncVar[Try[A]]
    task.runOnComplete(syncVar.put)(Scheduler(executionContext))
    syncVar.take.get
  }

  @Benchmark
  def monixAsyncCall(): Unit = {
    def loop(i: Int = 0, accumulator: Int = 0): monix.eval.Task[Int] = monix.eval.Task.async[Int] {
      (scheduler, callback) =>
        if (i < totalLoops) {
          loop(i + 1, accumulator + i).runAsync(callback)(scheduler)
        } else {
          monix.eval.Task(accumulator).runAsync(callback)(scheduler)
        }
    }
    import scala.concurrent.ExecutionContext.Implicits.global
    val result = blockingExecuteMonix(loop())
    assert(result == expectedResult)

  }

  @volatile var totalLoops = 1000

  val expectedResult = (0 until totalLoops).sum

}
