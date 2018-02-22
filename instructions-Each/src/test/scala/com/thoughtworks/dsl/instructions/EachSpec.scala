package com.thoughtworks.dsl.instructions

import org.scalatest.{FreeSpec, Matchers}
import Each.fork

/**
  * @author 杨博 (Yang Bo)
  */
class EachSpec extends FreeSpec with Matchers {
  type AsyncFunction[Domain, +A] = (A => Domain) => Domain

  "val in class" in {
    class C {
      val ascii: Set[Int] = Set(
        !Each(Seq(1, 2, 3, 2)) + 100
      )
    }

    (new C).ascii should be(Set(101, 102, 103))
  }

  "Given a continuation that uses Yield and Each expressions" - {

    def asyncFunction: AsyncFunction[Stream[String], Unit] = _ {
      !Yield("Entering asyncFunction")
      val subThreadId: Int = !fork(0, 1)
      !Yield(s"Fork sub-thread $subThreadId")
      !Yield("Leaving asyncFunction")
    }

    "When create a generator that contains Yield, Shift, and Each expressions" - {

      def generator: Stream[String] = {
        !Yield("Entering generator")
        val threadId = !fork(0, 1)
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
