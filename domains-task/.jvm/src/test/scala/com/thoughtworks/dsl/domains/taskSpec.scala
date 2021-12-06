package com.thoughtworks.dsl
package domains

import com.thoughtworks.dsl.Dsl.{!!, reset}
import org.scalatest.Assertion

import com.thoughtworks.dsl.keywords.{Using, Each, Fork}
import com.thoughtworks.dsl.domains.task._
import com.thoughtworks.dsl.keywords.Shift.implicitShift

import scala.collection.mutable.ArrayBuffer
import scala.util.control.TailCalls
import scala.util.{Failure, Success}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
final class taskSpec extends AsyncFreeSpec with Matchers {

  "tailRecurision" in Task.toFuture(Task.apply {
    def loop(i: Int = 0, accumulator: Int = 0): task.Task[Int] = _ {
      if (i < 1000) {
        !loop(i + 1, accumulator + i)
      } else {
        accumulator
      }
    }

    val result = !loop()
    result should be(499500)
  })

  "taskToFuture" in Task.toFuture(Task.apply {
    succeed
  })

  "loop" in Task.toFuture(Task.apply {

    val task1: Task[Int] = Task.now(1)

    val ts = Task.join {
      !Fork(0 until 10) + !task1
    }

    !ts should be(1 until 11)

  })

  "try" in Task.toFuture(Task.apply {
    class MyException extends Exception
    val task1: Task[Int] = Task.apply {
      throw new MyException
    }

    val task2 = Task.apply {
      val v =
        try {
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

    val task: Task[Unit] = Task.apply {

      logs += 0

      !Using(new AutoCloseable {
        logs += 10
        def close(): Unit = {
          logs += 20
        }
      })
      !Using(new AutoCloseable {
        logs += 11
        def close(): Unit = {
          logs += 21
        }
      })
      !Using(new AutoCloseable {
        logs += 12
        def close(): Unit = {
          logs += 22
        }
      })

      !Task.apply {
        logs += 3

        !Using(new AutoCloseable {
          logs += 40
          def close(): Unit = {
            logs += 50
          }
        })
        !Using(new AutoCloseable {
          logs += 41
          def close(): Unit = {
            logs += 51
          }
        })
        !Using(new AutoCloseable {
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

  "nested seq of task" in {

    def composeTask(t0: Task[Seq[Task[Seq[Task[Seq[Task[Seq[Float]]]]]]]]): Task[Seq[Seq[Seq[Seq[Float]]]]] = {
      // TODO: remove explicit type parameters when https://github.com/scala/bug/issues/11068 is fixed
      Task.join[Seq[Seq[Seq[Float]]], Seq[Seq[Seq[Seq[Float]]]]] {
        val t1 = !Each(!t0)
        !Task.join[Seq[Seq[Float]], Seq[Seq[Seq[Float]]]] {
          val t2 = !Each(!t1)
          !Task.join[Seq[Float], Seq[Seq[Float]]] {
            val t3 = !Each(!t2)
            !t3
          }
        }
      }
    }

    Task
      .toFuture(
        composeTask(
          Task.now(
            1 to 2 map { i =>
              Task.now(1 to 3 map { j =>
                Task.now(1 to 4 map { k =>
                  Task.now(1 to 5 map { l =>
                    (i * 1000 + j * 100 + k * 10 + l).toFloat
                  })
                })
              })
            }
          )
        )
      )
      .map { s =>
        s should be(
          1 to 2 map { i =>
            1 to 3 map { j =>
              1 to 4 map { k =>
                1 to 5 map { l =>
                  (i * 1000 + j * 100 + k * 10 + l).toFloat
                }
              }
            }
          }
        )

      }

  }
}
