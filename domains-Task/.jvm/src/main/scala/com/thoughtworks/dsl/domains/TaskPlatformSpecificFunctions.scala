package com.thoughtworks.dsl.domains
import com.thoughtworks.dsl.domains.Task
import scala.concurrent.SyncVar
import scala.util.Try
import scala.util.control.TailCalls
import scala.concurrent.duration.Duration
import com.thoughtworks.dsl.domains.Continuation
private[domains] trait TaskPlatformSpecificFunctions { this: Task.type =>

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
}
