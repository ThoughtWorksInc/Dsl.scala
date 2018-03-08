package com.thoughtworks.dsl
package benchmarks

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.benchmarks.RaiiBenchmark.IntException
import com.thoughtworks.dsl.domains.Raii

import scala.util.{Success, Try}
import com.thoughtworks.dsl.domains.Raii.Task
import monix.execution.{Cancelable, Scheduler}
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, State}

import scala.annotation.tailrec
import scala.concurrent.SyncVar
import scala.util.control.NoStackTrace
object RaiiBenchmark {
  final class IntException(val n: Int) extends Exception with NoStackTrace
}
@State(Scope.Benchmark)
class RaiiBenchmark {

  @Benchmark
  def raiiStackedCall(): Unit = {
    def loop(i: Int = 0): domains.Raii.Task[Int] = _ {
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
  def raiiTailCall(): Unit = {
    def loop(i: Int = 0, accumulator: Int = 0): domains.Raii.Task[Int] = _ {
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
  def raiiExceptionHandling(): Unit = {
    def throwing(i: Int): domains.Raii.Task[Unit] = _ {
      error(i)
    }
    val tasks: Seq[domains.Raii.Task[Unit]] = (0 until totalLoops).map(throwing)

    def loop(i: Int = 0, accumulator: Int = 0): domains.Raii.Task[Int] = _ {
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
  def raiiAsyncCall(): Unit = {
    def loop(i: Int = 0, accumulator: Int = 0): domains.Raii.Task[Int] = { callback =>
      if (i < totalLoops) {
        loop(i + 1, accumulator + i).apply(callback)
      } else {
        Task.now(accumulator).apply(callback)
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

    val result = blockingAwaitMonix(loop())
    assert(result == expectedResult)

  }

  @volatile var totalLoops = 1000

  val expectedResult = (0 until totalLoops).sum

}
