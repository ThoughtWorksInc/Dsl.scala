package com.thoughtworks.dsl
package benchmarks

import scala.util.{Success, Try}
import com.thoughtworks.dsl.domains.Raii.Task
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import scala.concurrent.SyncVar

@State(Scope.Benchmark)
class RaiiBenchmark {

  @Benchmark
  def raiiPingPong(): Unit = {

    def ping(n: Long, accumulator: Long = 0L): domains.Raii.Task[Long] = _ {
      if (n > 0) {
        !pong(n - 1, accumulator + n)
      } else {
        accumulator
      }
    }

    def pong(n: Long, accumulator: Long = 0L): domains.Raii.Task[Long] = _ {
      !ping(n - 1, accumulator + n)
    }

    val result = ping(1000L).blockingAwait()
    assert(result == 500500L)
  }

  private def blockingAwaitMonix[A](task: monix.eval.Task[A]): A = {
    import monix.execution.Scheduler.Implicits.global
    val syncVar = new SyncVar[Try[A]]
    task.runOnComplete(syncVar.put)
    syncVar.take.get
  }

  @Benchmark
  def monixPingPong(): Unit = {
    def ping(n: Long, accumulator: Long = 0L): monix.eval.Task[Long] = {
      if (n > 0) {
        monix.eval.Task.suspend(
          pong(n - 1, accumulator + n)
        )
      } else {
        monix.eval.Task(accumulator)
      }
    }

    def pong(n: Long, accumulator: Long = 0L): monix.eval.Task[Long] = {
      monix.eval.Task.suspend(
        ping(n - 1, accumulator + n)
      )
    }

    blockingAwaitMonix(ping(1000L)) == 500500L

  }

  @Benchmark
  def monixUnsafePingPong(): Unit = {
    def ping(n: Long, accumulator: Long = 0L): monix.eval.Task[Long] = {
      if (n > 0) {
        pong(n - 1, accumulator + n)
      } else {
        monix.eval.Task(accumulator)
      }
    }

    def pong(n: Long, accumulator: Long = 0L): monix.eval.Task[Long] = {
      ping(n - 1, accumulator + n)
    }

    blockingAwaitMonix(ping(1000L)) == 500500L

  }

}
