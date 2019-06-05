package com.thoughtworks.dsl.keywords

import java.util.concurrent.LinkedBlockingQueue

import org.scalatest.{FreeSpec, Matchers}
import com.thoughtworks.dsl.domains.task.Task
import scala.concurrent.duration._

class UsingSpec extends FreeSpec with Matchers {

  "scopeExit" - {
    import Using.scopeExit

    "execute sequence: function" in {
      val queue = new LinkedBlockingQueue[Int]()

      def run() = {
        !scopeExit(() => queue.offer(1))
        queue.offer(2)
      }

      run()

      queue.poll() should be(2)
      queue.poll() should be(1)
    }

    "execute sequence: function contains call-by-name function call" in {
      val queue = new LinkedBlockingQueue[Int]()

      def runOp(op: => Unit): Unit = op

      def run() = {
        !scopeExit(() => queue.offer(1))
        runOp {
          !scopeExit(() => queue.offer(3))
        }
        queue.offer(2)
      }

      run()

      queue.poll() should be(3)
      queue.poll() should be(2)
      queue.poll() should be(1)
    }

    "execute sequence: function contains call of function accepts function literal execute sequence" in {
      val queue = new LinkedBlockingQueue[Int]()

      def runOp(op: () => Unit): Unit = op()

      def run() = {
        !scopeExit(() => queue.offer(1))
        runOp(() => !scopeExit(() => queue.offer(3)))
        queue.offer(2)
      }

      run()

      queue.poll() should be(3)
      queue.poll() should be(2)
      queue.poll() should be(1)
    }

    "execute sequence: task" in {
      val queue = new LinkedBlockingQueue[Int]()

      def run() = Task {
        !scopeExit(() => queue.offer(1))
        queue.offer(2)
      }

      Task.blockingAwait(run(), 1.minute)

      queue.poll() should be(2)
      queue.poll() should be(1)
    }


    "execute sequence: task contains call-by-name function call" in {
      val queue = new LinkedBlockingQueue[Int]()

      def runTask(op: => Task[Unit]): Unit = Task.blockingAwait(op, 1.minute)

      def run() = Task {
        !scopeExit(() => queue.offer(1))
        runTask {
          Task(!scopeExit(() => queue.offer(3)))
        }
        queue.offer(2)
      }

      Task.blockingAwait(run(), 1.minute)

      queue.poll() should be(3)
      queue.poll() should be(2)
      queue.poll() should be(1)
    }

    "execute sequence: task contains call of function accepts function literal execute sequence" in {
      val queue = new LinkedBlockingQueue[Int]()

      def runOp(op: () => Task[Unit]): Unit = Task.blockingAwait(op(), 1.minute)

      def run() = {
        !scopeExit(() => queue.offer(1))
        runOp(() => Task(!scopeExit(() => queue.offer(3))))
        queue.offer(2)
      }

      run()

      queue.poll() should be(3)
      queue.poll() should be(2)
      queue.poll() should be(1)
    }

  }

}
