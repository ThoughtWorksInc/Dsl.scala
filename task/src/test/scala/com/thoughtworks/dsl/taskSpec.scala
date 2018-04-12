package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.{!!, reset}
import org.scalatest.{Assertion, AsyncFreeSpec, Matchers}

import com.thoughtworks.dsl.keywords.{AutoClose, Each, Fork}
import com.thoughtworks.dsl.task._
import com.thoughtworks.dsl.keywords.Shift.implicitShift

import scala.collection.mutable.ArrayBuffer
import scala.util.control.TailCalls
import scala.util.{Failure, Success}

/**
  * @author 杨博 (Yang Bo)
  */
final class taskSpec extends AsyncFreeSpec with Matchers {

  "taskToFuture" in Task.toFuture(Task.reset {
    succeed
  })

  "loop" in Task.toFuture(Task.reset {

    val task1: Task[Int] = Task.now(1)

    val ts = Task.join {
      !Fork(0 until 10) + !task1
    }

    !ts should be(1 until 11)

  })

  "try" in Task.toFuture(Task.reset {
    class MyException extends Exception
    val task1: Task[Int] = Task.reset {
      throw new MyException
    }

    val task2 = Task.reset {
      val v = try {
        !task1
        "no exception"
      } catch {
        case myException: MyException =>
          "my exception"
      }

      s"try: $v"
    }

    !task2 should be("try: my exception")
  })

  "empty try" in {
    val logs = ArrayBuffer.empty[String]

    class MyException extends Exception {
      logs += "MyException"
    }
    val task1: Task[String] = _ {
      throw new MyException
    }

    val task2: Task[String] = _ {
      try {
        "no exception"
      } catch {
        case myException: MyException =>
          "my exception"
      }
      !task1
    }

    Task.onComplete(task2) {
      case Success(s) =>
        logs += s
        throw new AssertionError()
      case Failure(e) =>
        e should be(a[MyException])
        logs += "uncaught MyException"
    }
    logs should be(ArrayBuffer("MyException", "uncaught MyException"))
  }

  "autoClose" in {
    val logs = ArrayBuffer.empty[Int]

    val task: Task[Unit] = Task.reset {

      logs += 0

      !AutoClose(new AutoCloseable {
        logs += 10
        def close(): Unit = {
          logs += 20
        }
      })
      !AutoClose(new AutoCloseable {
        logs += 11
        def close(): Unit = {
          logs += 21
        }
      })
      !AutoClose(new AutoCloseable {
        logs += 12
        def close(): Unit = {
          logs += 22
        }
      })

      !Task.reset {
        logs += 3

        !AutoClose(new AutoCloseable {
          logs += 40
          def close(): Unit = {
            logs += 50
          }
        })
        !AutoClose(new AutoCloseable {
          logs += 41
          def close(): Unit = {
            logs += 51
          }
        })
        !AutoClose(new AutoCloseable {
          logs += 42
          def close(): Unit = {
            logs += 52
          }
        })

        logs += 6
      }

      logs += 7

    }

    Task.toFuture(task).map { _ =>
      logs should be(ArrayBuffer(0, 10, 11, 12, 3, 40, 41, 42, 6, 52, 51, 50, 7, 22, 21, 20))
    }

  }

}
