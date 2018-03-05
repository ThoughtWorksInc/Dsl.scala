package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl.reset
import org.scalatest.{Assertion, AsyncFreeSpec, Matchers}
import Raii.{Task, taskToFuture}
import com.thoughtworks.dsl.instructions.{AutoClose, Each, Fork}

import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo)
  */
final class taskSpec extends AsyncFreeSpec with Matchers {

  "taskToFuture" in taskToFuture(Task.reset {
    succeed
  })

  "loop" in taskToFuture(Task.reset {

    val task1: Task[Int] = Task.now(1)

    val ts = Task.join {
      !Fork(0 until 10) + !task1
    }

    !ts should be(1 until 11)

  })

  "try" in taskToFuture(_ {
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
    val task1: Task[String] = Task.reset {
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

    task2.onComplete({ s =>
      logs += s
      throw new AssertionError()
    }, { e =>
      e should be(a[MyException])
      logs += "uncaught MyException"
    })
    logs should be(ArrayBuffer("MyException", "uncaught MyException"))
  }

  "autoClose" in {
    val logs = ArrayBuffer.empty[Int]

    val task = taskToFuture[Unit] {
      _ {
        logs += 0
        try {

          logs += 1

          !AutoClose(new AutoCloseable {
            logs += 2
            def close(): Unit = {
              logs += 3
            }
          })

          logs += 4

        } finally {}
        logs += 5

      }
    }

    task.map { _ =>
      logs should be(ArrayBuffer(0, 1, 2, 4, 3, 5))
    }

  }

}
