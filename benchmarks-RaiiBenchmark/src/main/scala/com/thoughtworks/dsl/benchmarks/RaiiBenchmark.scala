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
    type Task[+A] = Stream[Int] !! Raii !! A
    val tasks: Seq[Task[Unit]] = (0 until totalLoops).map { i =>
      val task: Task[Unit] = _ {
        error(i)
      }

      task
    }

    def loop(i: Int = 0, accumulator: Int = 0): Task[Int] = _ {
      if (i < totalLoops) {
        val n = try {
          !instructions.Yield(0)
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
    loop().onComplete {
      case Success(result) =>
        assert(result == expectedResult)
        Stream.empty
    }.last
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
