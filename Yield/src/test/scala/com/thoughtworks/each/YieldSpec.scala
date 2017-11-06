package com.thoughtworks.each

import com.thoughtworks.each.Await.AsyncFunction
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class YieldSpec extends FreeSpec with Matchers {

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
        !Await(yield4243)
        !Yield(1)
        Stream.empty[Int]
      }

      "Then the generator should contains yield values" in {
        generator should be(Seq(0, 42, 43, 1))
      }

    }

  }
}
