package com.thoughtworks.dsl.instructions

import org.scalatest.{FreeSpec, Matchers}
import cats.instances.stream._
import com.thoughtworks.dsl.instructions.Shift.Continuation

/**
  * @author 杨博 (Yang Bo)
  */
class CatsFlatMapSpec extends FreeSpec with Matchers {

  "Given a continuation that uses Yield and CatsFlatMap expressions" - {

    def asyncFunction: Continuation[Stream[String], Unit] = _ {
      !Yield("Entering asyncFunction")
      val subThreadId = !CatsFlatMap(Stream(0, 1))
      !Yield(s"Fork sub-thread $subThreadId")
      !Yield("Leaving asyncFunction")
    }

    "When create a generator that contains Yield, Shift, and CatsFlatMap expressions" - {

      def generator: Stream[String] = {
        !Yield("Entering generator")
        val threadId = !CatsFlatMap(Stream(0, 1))
        !Yield(s"Fork thread $threadId")
        !Shift(asyncFunction)
        Stream("Leaving generator")
      }

      "Then the generator should contains yield values" in {
        generator should be(
          Seq(
            /**/ "Entering generator",
            /****/ "Fork thread 0",
            /******/ "Entering asyncFunction",
            /********/ "Fork sub-thread 0",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /********/ "Fork sub-thread 1",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /****/ "Fork thread 1",
            /******/ "Entering asyncFunction",
            /********/ "Fork sub-thread 0",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /********/ "Fork sub-thread 1",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator"
          ))
      }

    }

  }

}
