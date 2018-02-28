package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.reset
import org.scalatest.{Assertion, AsyncFreeSpec, Matchers}
import task._
import com.thoughtworks.dsl.instructions.{Each, Fork}

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

  "try" in taskToFuture(Task.reset {
    class MyException extends Exception
    val task1: Task[Int] = Task.reset {
      throw new MyException
    }

    val task2 = Task.reset {
      try {
        !task1
        "no exception"
      } catch {
        case myException: MyException =>
          "my exception"
      }
    }

    !task2 should be("my exception")
  })

}
