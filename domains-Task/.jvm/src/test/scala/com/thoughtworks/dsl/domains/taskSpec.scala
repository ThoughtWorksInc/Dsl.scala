package com.thoughtworks.dsl
package domains

import com.thoughtworks.dsl.reset, reset._
import com.thoughtworks.dsl.Dsl.{!!}
import org.scalatest.Assertion
import scala.language.implicitConversions

import com.thoughtworks.dsl.keywords.{Using, Each}
import com.thoughtworks.dsl.domains._
import com.thoughtworks.dsl.keywords.Shift
import com.thoughtworks.dsl.keywords.Shift.given

import scala.collection.mutable.ArrayBuffer
import scala.util.control.TailCalls
import scala.util.{Failure, Success}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
final class taskSpec extends AsyncFreeSpec with Matchers {

  "tailRecurision" in Task.toFuture(Task {
    def loop(i: Int = 0, accumulator: Int = 0): Task[Int] = *[Task] {
      if (i < 10000) {
        !Shift(loop(i + 1, accumulator + i))
      } else {
        accumulator
      }
    }

    val result = !Shift(loop())
    result should be(49995000)
  })

  "taskToFuture" in Task.toFuture(Task {
    succeed
  })

  "loop" in Task.toFuture(Task {

    val task1: Task[Int] = Task.now(1)

    val ts = *[Task]/* .join */ apply Seq {
      !Each(0 until 10) + !Shift(task1)
    }

    !Shift(ts) should be(1 until 11)

  })

  "*[Task] does not necessarily suspend or catch exceptions" in {
    class MyException extends Exception
    def task1: Task[Int] = *[Task] {
      throw new MyException
    }
    a[MyException] should be thrownBy task1
  }

  "try" in Task.toFuture(Task {
    class MyException extends Exception
    val task1: Task[Int] = Task {
      throw new MyException
    }

    val task2 = Task {
      val v =
        try {
          !Shift(task1)
          "no exception"
        } catch {
          case myException: MyException =>
            "my exception"
        }

      s"try: $v"
    }

    !Shift(task2) should be("try: my exception")
  })

  "empty try" in {
    val logs = ArrayBuffer.empty[String]

    class MyException extends Exception {
      logs += "MyException"
    }
    val task1: Task[String] = _ {
      throw new MyException
    }

    val task2: Task[String] = Task {
      try {
        "no exception"
      } catch {
        case myException: MyException =>
          "my exception"
      }
      !Shift(task1)
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

    // TODO: Re-implement Using to support `Task {}` instead of `*[Task]`
    val task: Task[Unit] = *[Task] {

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

      // TODO: Re-implement Using to support `Task{}`
      !Shift(*[Task] {
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
      })

      logs += 7

    }

    Task.toFuture(task).map { _ =>
      logs should be(ArrayBuffer(0, 10, 11, 12, 3, 40, 41, 42, 6, 52, 51, 50, 7, 22, 21, 20))
    }

  }

  // Task.join is not supported any more
  "nested seq of task" ignore {

    def composeTask(t0: Task[Seq[Task[Seq[Task[Seq[Task[Seq[Float]]]]]]]]): Task[Seq[Seq[Seq[Seq[Float]]]]] = {
      // TODO: remove explicit type parameters when https://github.com/scala/bug/issues/11068 is fixed
      *[Task]/*.join*/ apply Seq {
        val t1 = !Each(!Shift(t0))
        !Shift(*[Task]/*.join*/ apply Seq {
          val t2 = !Each(!Shift(t1))
          !Shift(*[Task]/*.join*/ apply Seq {
            val t3 = !Each(!Shift(t2))
            !Shift(t3)
          })
        })
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
