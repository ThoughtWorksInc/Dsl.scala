package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.reset
import org.scalatest.{Assertion, AsyncFreeSpec, Matchers}
import task._
import com.thoughtworks.dsl.instructions.{Each, Fork}

/**
  * @author 杨博 (Yang Bo)
  */
final class taskSpec extends AsyncFreeSpec with Matchers {

  "taskToFuture" in {
    succeed
  }

  "loop" in taskToFuture {

    val task1: Task[Int] = 1

    val ts = Task.join {
      !Fork(0 until 10) + !task1
    }

    !ts should be(1 until 11)

  }

}
