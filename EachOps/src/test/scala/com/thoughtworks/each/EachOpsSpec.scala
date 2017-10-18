package com.thoughtworks.each

import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class EachOpsSpec extends FreeSpec with Matchers {
  import EachOpsSpec._

  "Given a case class that yields a Stream" - {

    final case class Yield[Element](element: Element) extends Continuation[Stream[Element], Unit] {
      @inline override def apply(continue: Unit => Stream[Element]): Stream[Element] = {
        Stream.cons(element, continue(()))
      }
    }

    "When create a generator that contains conditional Yield" - {
      def generator: Stream[Int] = {
        if (false) {
          Yield(0).!
        }
        if (true) {
          Yield(1).!
        }
        if ({ Yield(2).!; false }) {
          Yield(3).!
        } else {
          Yield(4).!
        }
        Stream.empty
      }

      "Then the generator should contains values in selected branches" in {
        generator should be(Seq(1, 2, 4))
      }

    }

    "When create a continuation that uses Yield" - {

      def yield42: Continuation[Stream[Int], Unit] = _ {
        Yield(42).!
        Yield(43).!
      }

      "and create a generator that contains multiple Yield expression followed by a bang notation and a Stream.empty" - {

        def generator: Stream[Int] = {
          Yield(0).!
          yield42.!
          Yield(1).!
          Stream.empty
        }

        "Then the generator should contains yield values" in {
          generator should be(Seq(0, 42, 43, 1))
        }

      }

    }

  }
}

object EachOpsSpec {
  type Continuation[R, +A] = (A => R) => R

  implicit final class ContinuationOps[R, A](val underlying: Continuation[R, A]) extends AnyVal with EachOps[A] {
    def apply(continue: A => R): R = underlying(continue)
  }

}
