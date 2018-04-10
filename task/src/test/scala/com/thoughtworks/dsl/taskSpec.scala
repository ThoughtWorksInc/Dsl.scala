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

  "taskToFuture" in Task.reset {
    succeed
  }.toFuture

  "loop" in Task.reset {

    val task1: Task[Int] = Task.now(1)

    val ts = Task.join {
      !Fork(0 until 10) + !task1
    }

    !ts should be(1 until 11)

  }.toFuture

  "try" in Task.reset {
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
  }.toFuture

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

    task2.run {
      case Success(s) =>
        logs += s
        throw new AssertionError()
      case Failure(e) =>
        e should be(a[MyException])
        logs += "uncaught MyException"
        TailCalls.done(())
    }
    logs should be(ArrayBuffer("MyException", "uncaught MyException"))
  }

  // FIXME:
  "autoClose" ignore {
    val logs = ArrayBuffer.empty[Int]

    val task: Task[Unit] = Task.reset {
      try {

        logs += 0

        !AutoClose(new AutoCloseable {
          logs += 1
          def close(): Unit = {
            logs += 2
          }
        })
        !AutoClose(new AutoCloseable {
          logs += 1
          def close(): Unit = {
            logs += 2
          }
        })
        !AutoClose(new AutoCloseable {
          logs += 1
          def close(): Unit = {
            logs += 2
          }
        })
        try {
          try {

            logs += 3

            !AutoClose(new AutoCloseable {
              logs += 4
              def close(): Unit = {
                logs += 5
              }
            })
            !AutoClose(new AutoCloseable {
              logs += 4
              def close(): Unit = {
                logs += 5
              }
            })
            !AutoClose(new AutoCloseable {
              logs += 4
              def close(): Unit = {
                logs += 5
              }
            })

            logs += 6

          } finally {}
        } finally {}
        logs += 7

      } finally {}
    }

    task.toFuture.map { _ =>
      logs should be(ArrayBuffer(0, 1, 1, 1, 3, 4, 4, 4, 6, 5, 5, 5, 7, 2, 2, 2))
    }

  }

}
