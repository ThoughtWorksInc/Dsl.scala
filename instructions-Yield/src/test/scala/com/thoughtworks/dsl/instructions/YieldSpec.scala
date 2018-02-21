package com.thoughtworks.dsl.instructions

import org.scalatest.{FreeSpec, Matchers}

import scala.annotation.tailrec

/**
  * @author 杨博 (Yang Bo)
  */
class YieldSpec extends FreeSpec with Matchers {
  type AsyncFunction[Domain, +A] = (A => Domain) => Domain

  "while" - {
    "false" in {
      def whileFalse: Stream[Int] = {
        while (false) {
          !Yield(100)
        }
        Stream.empty[Int]
      }

      whileFalse should be(Stream.empty)
    }
  }

  "match/case" in {

    def loop(i: Int): Stream[Int] = {
      i match {
        case 100 =>
          Stream.empty
        case _ =>
          !Yield(i)
          loop(i + 1)
      }
    }

    loop(90) should be(Stream(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))

  }

  "recursive" in {
    def loop(i: Int): Stream[Int] = {
      if (i < 100) {
        !Yield(i)
        loop(i + 1)
      } else {
        Stream.empty
      }
    }

    loop(90) should be(Stream(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))

  }

  "Given a generator that contains conditional Yield" - {
    def generator = {
      if (false) {
        !Yield(0)
      }
      if (true) {
        !Yield(1)
      }
      if ({ !Yield(2); false }) {
        !Yield(3)
      } else {
        !Yield(4)
      }
      Stream.empty[Int]
    }

    "Then the generator should contains values in selected branches" in {
      generator should be(Seq(1, 2, 4))
    }

  }

  "Given a continuation that uses Yield" - {

    def yield4243: AsyncFunction[Stream[Int], Unit] = _ {
      !Yield(42)
      !Yield(43)
    }

    "when create a generator that contains multiple Yield expression followed by a bang notation and a Stream.empty" - {

      def generator: Stream[Int] = {
        !Yield(0)
        !Shift(yield4243)
        !Yield(1)
        Stream.empty[Int]
      }

      "Then the generator should contains yield values" in {
        generator should be(Seq(0, 42, 43, 1))
      }

    }

  }
}
