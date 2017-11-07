package com.thoughtworks.dsl.instructions

import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class EachSpec extends FreeSpec with Matchers {
  type AsyncFunction[Domain, +A] = (A => Domain) => Domain

  "Given a continuation that uses Yield and Each expressions" - {

    def asyncFunction: AsyncFunction[Stream[String], Unit] = _ {
      !Yield("Entering asyncFunction")
      val subThreadId:Int = !Each(0, 1)
      !Yield(s"Fork sub-thread $subThreadId")
      !Yield("Leaving asyncFunction")
    }

    "When create a generator that contains Yield, Await, and Each expressions" - {

      def generator: Stream[String] = {
        !Yield("Entering generator")
        val threadId = !Each(0, 1)
        !Yield(s"Fork thread $threadId")
        !Await(asyncFunction)
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
